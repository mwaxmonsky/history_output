!!XXgoldyXX: To do, statistics output
module hist_hash_table

   implicit none
   private

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !
   !  Hashing.
   !
   !  Accelerate processing (e.g., of outfld) by using a hash function of
   !    the field name
   !
   !  Note: the hashing logic will fail if any of the following are true:
   !
   !         1) The lower bound on the dimension of 'masterlist' is less than 1.
   !
   !         2) 'outfld' is called with field names that are not defined on
   !            masterlist.  This applies to both initial/branch and restart
   !            runs.
   !
   !         3) An inconsistency between a field's file active flag
   !            'masterlist(ff)%actflag(t)' and active fields read from
   !            restart files.
   !
   !         4) Invoking function 'gen_hash_key' before the primary and
   !            secondary hash tables have been created
   !            (routine bld_outfld_hash_tbls).
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   !
   !  User definable constants for hash and overflow tables.
   !  Define size of primary hash table (specified as 2**size).
   !
!   integer, parameter :: tbl_hash_pri_sz_lg2 = 16
   !
   !  Define size of overflow hash table % of primary hash table.
   !
!   integer, parameter :: tbl_hash_oflow_percent = 20
   !
   !  Do *not* modify the parameters below.
   !
!   integer, parameter :: tbl_hash_pri_sz = 2**tbl_hash_pri_sz_lg2
!   integer, parameter :: tbl_hash_oflow_sz = tbl_hash_pri_sz * (tbl_hash_oflow_percent/100.0_r8)
   !
   !  The primary and overflow tables are organized to mimimize space (read:
   !  try to maximimze cache line usage).
   !
   !  gen_hash_key(fieldname) will return an index on the interval
   !  [0 ... tbl_hash_pri_sz-1].
   !
   !
   !  Primary:
   !  gen_hash_key(fieldname)-------+     +----------+
   !                                |     |   -ii    | 1 ------>tbl_hash_oflow(ii)
   !                                |     +----------+
   !                                +-->  |    ff    | 2 ------>masterlist(ff)
   !                                      +----------+
   !                                      |          | ...
   !                                      +----------+
   !                                      |          | tbl_hash_pri_sz
   !                                      +----------+
   !
   !  Overflow (if tbl_hash_pri() < 0):
   !  tbl_hash_pri(gen_hash_key(fieldname))
   !                         |
   !                         |            +----------+
   !                         |            |     1    | 1  (one entry on O.F. chain)
   !                         |            +----------+
   !                         |            |    ff_m  | 2
   !                         |            +----------+
   !                         +--------->  |     3    | 3  (three entries on chain)
   !                                      +----------+
   !                                      |    ff_x  | 4
   !                                      +----------+
   !                                      |    ff_y  | 5
   !                                      +----------+
   !                                      |    ff_z  | 6
   !                                      +----------+
   !                                      |          | ...
   !                                      +----------+
   !                                      |          | tbl_hash_oflow_sz
   !                                      +----------+
   !
   !
   !
   !  Constants used in hashing function gen_hash_key.
   !  Note: if the constants in table 'tbl_gen_hash_key' below are modified,
   !        changes are required to routine 'gen_hash_key' because of specific
   !        logic in the routine that optimizes character strings of length 8.
   !

   type, abstract, public :: hist_hashable_t
      ! The hashable type is a base type that contains a hash key.
      integer, private :: hash_key_len = 0
   contains
      procedure                                  :: key_len
      procedure(hist_hashable_get_key), deferred :: key
   end type hist_hashable_t

   type, public, extends(hist_hashable_t) :: hist_hashable_char_t
      character(len=:), private, allocatable :: name
   contains
      procedure :: key => hist_hashable_char_get_key
   end type hist_hashable_char_t

   integer, parameter :: gen_hash_key_offset = 21467 ! z'000053db'

   integer, parameter :: tbl_max_idx = 15
   integer, parameter, dimension(0:tbl_max_idx) :: tbl_gen_hash_key =         &
        (/ 61, 59, 53, 47, 43, 41, 37, 31, 29, 23, 17, 13, 11, 7, 3, 1 /)

   integer, parameter :: table_factor_size = 8     ! Table size / # entries
   integer, parameter :: table_overflow_factor = 4 ! # entries / Overflow size

   type :: hash_overflow_entry
      ! An entry for a hash overflow table
      integer, pointer :: hentry(:) => NULL()
   end type hash_overflow_entry

   type, public :: hist_hash_table_t
      ! hist_hash_table_t contains all information to build and use a hash table
      ! It also keeps track of statistics such as collision frequency and size
      integer, private              :: table_size = -1
      integer, private              :: overflow_size = -1
      integer, private              :: key_offset = gen_hash_key_offset
      integer, private, allocatable :: primary_table(:)
      integer, private              :: next_overflow_index = 1
      type(hash_overflow_entry), private, allocatable :: overflow_table(:)
      ! Statistics
      integer, private :: num_keys = 0
      integer, private :: num_key_collisions = 0
      integer, private :: max_collision = 0
   contains
      procedure :: initialize   => hash_table_initialize_table
      procedure :: key_hash     => hash_table_key_hash
      procedure :: add_hash_key => hash_table_add_hash_key
      procedure :: table_index  => hash_table_table_index
   end type hist_hash_table_t

   ! Abstract interface for key procedure of hist_hashable class
   abstract interface
      function hist_hashable_get_key(hashable)
         import :: hist_hashable_t
         class(hist_hashable_t), intent(in) :: hashable
         character(len=:), allocatable      :: hist_hashable_get_key
      end function hist_hashable_get_key
   end interface

CONTAINS

   !#######################################################################
   integer function key_len(this)
      class(hist_hashable_t), intent(in) :: this

      key_len = this%hash_key_len
   end function key_len

   !#######################################################################

   function hist_hashable_char_get_key(hashable)
      ! Return the hashable char class key (name)
      class(hist_hashable_char_t), intent(in) :: hashable
      character(len=:), allocatable           :: hist_hashable_char_get_key

      hist_hashable_char_get_key = hashable%name
   end function hist_hashable_char_get_key

   !#######################################################################

   subroutine hash_table_initialize_table(this, tbl_size, ovflw_size, key_off)
      ! Initialize this table.
      ! Dummy arguments
      class(hist_hash_table_t)      :: this
      integer,           intent(in) :: tbl_size   ! new table size
      integer,           intent(in) :: ovflw_size ! overflow table size
      integer, optional, intent(in) :: key_off    ! key offset
      ! Local variable
      integer                       :: index

      ! Clear this table so it can be initialized
      if (allocated(this%primary_table)) then
         deallocate(this%primary_table)
      end if
      if (allocated(this%overflow_table)) then
         do index = 1, size(this%overflow_table, 1)
            if (associated(this%overflow_table(index)%hentry)) then
               deallocate(this%overflow_table(index)%hentry)
               nullify(this%overflow_table(index)%hentry)
            end if
         end do
         deallocate(this%overflow_table)
      end if
      this%table_size = tbl_size
      allocate(this%primary_table(this%table_size))
      this%primary_table = 0
      this%overflow_size = ovflw_size
      allocate(this%overflow_table(this%overflow_size))
      if (present(key_off)) then
         this%key_offset = key_off
      end if
   end subroutine hash_table_initialize_table

   !#######################################################################

   integer function hash_table_key_hash(this, string) result(hash_key)
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Generate a hash key on the interval [0 .. tbl_hash_pri_sz-1]
      !          given a character string.
      !
      ! Algorithm is a variant of perl's internal hashing function.
      !
      !-----------------------------------------------------------------------
      !
      !
      !  Arguments:
      !
      class(hist_hash_table_t)     :: this
      character(len=*), intent(in) :: string
      !
      !  Local.
      !
      integer :: hash
      integer :: index
      integer :: ind_fact
      integer :: hash_fact

      hash = this%key_offset
      ind_fact = 0
      do index = 1, len(string)
         ind_fact = ind_fact + 1
         if (ind_fact > tbl_max_idx) then
            ind_fact = 1
         end if
         hash_fact = tbl_gen_hash_key(ind_fact)
         hash = ieor(hash, (ichar(string(index:index)) * hash_fact))
      end do

      hash_key = iand(hash, this%table_size - 1)

   end function hash_table_key_hash

   !#######################################################################

   integer function hash_table_table_index(this, hash_arr, key,               &
        errmsg) result(indx)
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Return the the index of <key> in <hash_arr>
      !
      !          If the object is not found, return -1.
      !
      !          <hash_arr> is an array of hashable objects
      !          For correct usage, all of the keys in <hash_arr>
      !          MUST have been added to <this> table.
      !
      !-----------------------------------------------------------------------
      !
      !  Arguments:
      !
      class(hist_hash_table_t)                :: this
      class(hist_hashable_t),     intent(in)  :: hash_arr(:)
      character(len=*),           intent(in)  :: key
      character(len=*), optional, intent(out) :: errmsg
      !
      !  Local.
      !
      integer          :: hash_key
      integer          :: ovflw_cind ! location of number of overflow entries
      integer          :: ind_ovflw  ! Index of overflow chain in overflow table
      integer          :: num_ovflw  ! Number of entries on overflow chain
      integer          :: test_ind   ! Test index for overflow entries
      integer, pointer :: ovflw(:)   ! Overflow chain
      character(len=*), parameter :: subname = 'HASH_TABLE_TABLE_INDEX'

      errmsg = ''
      hash_key = this%key_hash(key)
      indx = this%primary_table(hash_key)
      if (indx < 0) then
         ovflw_cind = abs(indx)
         ovflw => this%overflow_table(ovflw_cind)%hentry
         if (.not. associated(ovflw)) then
            if (present(errmsg)) then
               write(errmsg, *) subname, ": Empty overflow entry for '",      &
                    trim(key), "'"
            end if
         else
            num_ovflw = size(ovflw, 1)
            do ind_ovflw = 1, num_ovflw
               test_ind = ovflw(ind_ovflw)
               if (hash_arr(test_ind)%key() == key) then
                  indx = test_ind
                  exit
               end if
            end do
         end if
      end if

      if ((indx <= 0) .and. present(errmsg)) then
         if (indx == 0) then
            write(errmsg, *) subname, ": No entry for '", trim(key), "'"
         else if (len_trim(errmsg) == 0) then
            write(errmsg, *) subname, ": No overflow entry for '",            &
                 trim(key), "'"
         end if ! No else, we have already recorded an error
      else if ((hash_arr(test_ind)%key() /= key) .and. present(errmsg)) then
         write(errmsg, *) subname, ": error finding field '", trim(key), "'"
      end if

   end function hash_table_table_index

   !#######################################################################

   subroutine hash_table_add_hash_key(this, key, keyval)
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Add <keyval> to this hash table using key, <key>
      !
      !
      !-----------------------------------------------------------------------

      !  Dummy arguments:
      class(hist_hash_table_t)                :: this
      character(len=*),           intent(in)  :: key
      integer,                    intent(in)  :: keyval
      ! Local variables
      integer                     :: hash_ind
      integer                     :: ovflw_ind
      integer                     :: ovflw_len
      integer,          pointer   :: curr_ovflw(:)
      character(len=*), parameter :: subname = 'HASH_TABLE_ADD_HASH_KEY'

      hash_ind = this%key_hash(key)
      if (this%primary_table(hash_ind) == 0) then
         ! No entry, add the entry here
         this%primary_table(hash_ind) = keyval
      else if (this%primary_table(hash_ind) < 0) then
         ! We already have a collision here, add a new one
         ovflw_ind = abs(hash_ind)
         curr_ovflw => this%overflow_table(ovflw_ind)%hentry
         ovflw_len = size(curr_ovflw, 1) + 1
         allocate(this%overflow_table(ovflw_ind)%hentry(ovflw_len))
         this%overflow_table(ovflw_ind)%hentry(1:ovflw_len-1) = curr_ovflw(:)
         this%overflow_table(ovflw_ind)%hentry(ovflw_len) = keyval
         this%max_collision = MAX(this%max_collision, ovflw_len)
      else
         ! We have a new collision, create an overflow entry
         ovflw_ind = this%next_overflow_index
         this%next_overflow_index = this%next_overflow_index + 1
         ovflw_len = 2
         allocate(this%overflow_table(ovflw_ind)%hentry(ovflw_len))
         this%overflow_table(ovflw_ind)%hentry(1) = this%primary_table(hash_ind)
         this%overflow_table(ovflw_ind)%hentry(ovflw_len) = keyval
         this%max_collision = MAX(this%max_collision, ovflw_len)
         this%num_key_collisions = this%num_key_collisions + 1
         this%primary_table(hash_ind) = -ovflw_ind
      end if
      this%num_keys = this%num_keys + 1

   end subroutine hash_table_add_hash_key

end module hist_hash_table
