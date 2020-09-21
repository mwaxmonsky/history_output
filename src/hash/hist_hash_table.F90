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
   contains
      procedure(hist_hashable_get_key), deferred :: key
   end type hist_hashable_t

   type, public, extends(hist_hashable_t) :: hist_hashable_char_t
      character(len=:), private, allocatable :: name
   contains
      procedure :: key => hist_hashable_char_get_key
   end type hist_hashable_char_t

   public :: new_hashable_char

   type, public, extends(hist_hashable_t) :: hist_hashable_int_t
      integer, private :: value
   contains
      procedure :: key => hist_hashable_int_get_key
      procedure :: val => hist_hashable_int_get_val
   end type hist_hashable_int_t

   public :: new_hashable_int

   integer, parameter :: gen_hash_key_offset = 21467 ! z'000053db'

   integer, parameter :: tbl_max_idx = 15
   integer, parameter, dimension(0:tbl_max_idx) :: tbl_gen_hash_key =         &
        (/ 61, 59, 53, 47, 43, 41, 37, 31, 29, 23, 17, 13, 11, 7, 3, 1 /)

   integer, parameter :: table_factor_size = 8     ! Table size / # entries
   integer, parameter :: table_overflow_factor = 4 ! # entries / Overflow size

   type :: table_entry_t
      ! Any table entry contains a key and a value
      class(hist_hashable_t), pointer             :: entry_value => NULL()
      type(table_entry_t),    pointer             :: next => NULL()
   contains
      final :: finalize_table_entry
   end type table_entry_t

   type, public :: hist_hash_table_t
      ! hist_hash_table_t contains all information to build and use a hash table
      ! It also keeps track of statistics such as collision frequency and size
      integer, private                          :: table_size = -1
      integer, private                          :: overflow_size = -1
      integer, private                          :: key_offset = gen_hash_key_offset
      type(table_entry_t), private, allocatable :: primary_table(:)
 ! Statistics
      integer, private                          :: num_keys = 0
      integer, private                          :: num_key_collisions = 0
      integer, private                          :: max_collision = 0
   contains
      procedure :: initialize   => hash_table_initialize_table
      procedure :: key_hash     => hash_table_key_hash
      procedure :: add_hash_key => hash_table_add_hash_key
      procedure :: table_value  => hash_table_table_value
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

   function new_hashable_char(name_in)
      type(hist_hashable_char_t)   :: new_hashable_char
      character(len=*), intent(in) :: name_in

      new_hashable_char%name = name_in
   end function new_hashable_char

   !#######################################################################

   function hist_hashable_char_get_key(hashable)
      ! Return the hashable char class key (name)
      class(hist_hashable_char_t), intent(in) :: hashable
      character(len=:), allocatable           :: hist_hashable_char_get_key

      hist_hashable_char_get_key = hashable%name
   end function hist_hashable_char_get_key

   !#######################################################################

   function new_hashable_int(value_in)
      type(hist_hashable_int_t) :: new_hashable_int
      integer, intent(in)       :: value_in

      new_hashable_int%value = value_in
   end function new_hashable_int

   !#######################################################################

   function hist_hashable_int_get_key(hashable)
      ! Return the hashable int class key (value ==> string)
      class(hist_hashable_int_t), intent(in) :: hashable
      character(len=:), allocatable          :: hist_hashable_int_get_key

      character(len=32) :: key_str

      write(key_str, '(i0)') hashable%val()
      hist_hashable_int_get_key = trim(key_str)
   end function hist_hashable_int_get_key

   !#######################################################################

   integer function hist_hashable_int_get_val(hashable)
      ! Return the hashable int class value
      class(hist_hashable_int_t), intent(in) :: hashable

      hist_hashable_int_get_val = hashable%value
   end function hist_hashable_int_get_val

   !#######################################################################

   subroutine finalize_table_entry(te)
      type(table_entry_t)          :: te

      nullify(te%entry_value) ! We do not own the memory
      if (associated(te%next)) then
         deallocate(te%next) ! This should invoke finalize recursively
         nullify(te%next)
      end if

   end subroutine finalize_table_entry

   !#######################################################################

   subroutine hash_table_initialize_table(this, tbl_size, key_off)
      ! Initialize this table.
      ! Dummy arguments
      class(hist_hash_table_t)      :: this
      integer,           intent(in) :: tbl_size   ! new table size
      integer, optional, intent(in) :: key_off    ! key offset
      ! Local variable
      integer                       :: index

      ! Clear this table so it can be initialized
      if (allocated(this%primary_table)) then
         deallocate(this%primary_table)
      end if
      this%table_size = tbl_size
      allocate(this%primary_table(this%table_size))
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
      do index = 1, len_trim(string)
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

   function hash_table_table_value(this, key, errmsg) result(tbl_val)
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Return the the key value of <key>
      !
      !          If the object is not found, return NULL
      !
      !-----------------------------------------------------------------------
      !
      !  Arguments:
      !
      class(hist_hash_table_t)                      :: this
      character(len=*),                 intent(in)  :: key
      character(len=*),       optional, intent(out) :: errmsg
      class(hist_hashable_t), pointer               :: tbl_val
      !
      !  Local.
      !
      integer                        :: hash_key
      type(table_entry_t), pointer   :: next_ptr
      character(len=*),    parameter :: subname = 'HASH_TABLE_TABLE_INDEX'

      errmsg = ''
      nullify(tbl_val)
      hash_key = this%key_hash(key)
      if (this%primary_table(hash_key)%entry_value%key() == trim(key)) then
         tbl_val => this%primary_table(hash_key)%entry_value
      else
         next_ptr => this%primary_table(hash_key)%next
         do
            if (associated(next_ptr)) then
               if (next_ptr%entry_value%key() == trim(key)) then
                  tbl_val => next_ptr%entry_value
                  exit
               end if
               next_ptr => next_ptr%next
            else
               exit
            end if
         end do
      end if

      if ((.not. associated(tbl_val)) .and. present(errmsg)) then
         write(errmsg, *) subname, ": No entry for '", trim(key), "'"
      end if

   end function hash_table_table_value

   !#######################################################################

   subroutine hash_table_add_hash_key(this, newval, errmsg)
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Add <newval> to this hash table using its key
      !          Its key must not be an empty string
      !          It is an error to try to add a key more than once
      !
      !
      !-----------------------------------------------------------------------

      !  Dummy arguments:
      class(hist_hash_table_t)                      :: this
      class(hist_hashable_t), pointer               :: newval
      character(len=*),       optional, intent(out) :: errmsg
      ! Local variables
      integer                          :: hash_ind
      integer                          :: ovflw_len
      character(len=:),    allocatable :: newkey
      type(table_entry_t), pointer     :: next_ptr
      type(table_entry_t), pointer     :: new_entry
      character(len=*),    parameter   :: subname = 'HASH_TABLE_ADD_HASH_KEY'

      newkey = newval%key()
      hash_ind = this%key_hash(newkey)
      ! Check for this entry
      if (associated(this%table_value(newkey))) then
         if (present(errmsg)) then
            write(errmsg, *) subname, " ERROR: key, '", newkey,               &
                 "' already in table"
         end if
      else
         ASSOCIATE(tbl_entry => this%primary_table(hash_ind))
            if (associated(tbl_entry%entry_value)) then
               ! We have a collision, make a new entry
               allocate(new_entry)
               new_entry%entry_value => newval
               ! Now, find a spot
               if (associated(tbl_entry%next)) then
                  ovflw_len = 1
                  next_ptr => tbl_entry%next
                  do
                     if (associated(next_ptr%next)) then
                        ovflw_len = ovflw_len + 1
                        next_ptr => next_ptr%next
                     else
                        exit
                     end if
                  end do
                  ovflw_len = ovflw_len + 1
                  next_ptr%next => new_entry
               else
                  this%num_key_collisions = this%num_key_collisions + 1
                  tbl_entry%next => new_entry
                  ovflw_len = 1
               end if
               this%max_collision = MAX(this%max_collision, ovflw_len)
            end if
         END ASSOCIATE
      end if
      this%num_keys = this%num_keys + 1

   end subroutine hash_table_add_hash_key

end module hist_hash_table
