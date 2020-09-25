module hist_api

   use ISO_FORTRAN_ENV, only: REAL64, REAL32, INT32, INT64
   use hist_hashable,   only: hist_hashable_t
   use hist_field,      only: hist_field_info_t
   use hist_buffer,     only: hist_buffer_t, buffer_factory
   use hist_buffer,     only: hist_buffer_1dreal32_inst_t
   use hist_buffer,     only: hist_buffer_1dreal64_inst_t

   implicit none
   private

   ! Public API interfaces
   public :: hist_new_field         ! Allocate a hist_field_info_t object
   public :: hist_new_buffer        ! Create a new field buffer
   public :: hist_buffer_accumulate ! Accumulate a new field state
!   public :: hist_buffer_norm_value ! Return current normalized field state
!   public :: hist_buffer_clear      ! Clear buffer accumulation state
!   public :: hist_buffer_accum_type ! String value of accumulation type

   ! Interfaces for public interfaces
   interface hist_buffer_accumulate
      module procedure hist_buffer_accumulate_1dreal32
      module procedure hist_buffer_accumulate_1dreal64
   end interface hist_buffer_accumulate

!   interface hist_buffer_norm_value
!      module procedure hist_buffer_norm_value_1dreal32
!      module procedure hist_buffer_norm_value_1dreal64
!   end interface hist_buffer_norm_value

CONTAINS

   !#######################################################################

   logical function have_error(errmsg)
      ! Return .true. iff <errmsg> is present and contains text
      character(len=*), optional, intent(in) :: errmsg

      have_error = present(errmsg)
      if (have_error) then
         have_error = len_trim(errmsg) > 0
      end if
   end function have_error

   !#######################################################################

   subroutine hist_new_field(new_field, diag_name_in, std_name_in,            &
        long_name_in, units_in, errmsg)
      type(hist_field_info_t), pointer               :: new_field
      character(len=*),                  intent(in)  :: diag_name_in
      character(len=*),                  intent(in)  :: std_name_in
      character(len=*),                  intent(in)  :: long_name_in
      character(len=*),                  intent(in)  :: units_in
      character(len=*),        optional, intent(out) :: errmsg

      integer :: astat
      character(len=*), parameter :: subname = 'hist_new_field'

      errmsg = ''
      if (associated(new_field)) then
         deallocate(new_field, stat=astat)
         if ((astat /= 0) .and. present(errmsg)) then
            write(errmsg, *) subname, ' Unable to deallocate <new_field>'
         end if
      end if
      if (.not. have_error(errmsg)) then
         allocate(new_field, stat=astat)
         if ((astat /= 0) .and. present(errmsg)) then
            write(errmsg, *) subname, ' Unable to allocate <new_field>'
         end if
      end if
      if (.not. have_error(errmsg)) then
         call hist_field_initialize(new_field, diag_name_in, std_name_in,     &
              long_name_in, units_in, errmsg)
         if (have_error(errmsg)) then
            write(errmsg(len_trim(errmsg)+1:), *) ', called from ', subname
         end if
      end if
   end subroutine hist_new_field

   !#######################################################################

   subroutine hist_new_buffer(field, buff_shape, buff_kind, horiz_axis_ind,   &
        accum_type, output_vol, buffer, errmsg, block_ind, block_sizes)
      ! Dummy arguments
      class(hist_hashable_t), pointer               :: field
      integer,                          intent(in)  :: buff_shape(:)
      integer,                          intent(in)  :: buff_kind
      integer,                          intent(in)  :: horiz_axis_ind
      character(len=*),                 intent(in)  :: accum_type
      integer,                          intent(in)  :: output_vol
      class(hist_buffer_t),   pointer,  intent(out) :: buffer
      character(len=*),       optional, intent(out) :: errmsg
      integer,                optional, intent(in)  :: block_ind
      integer,                optional, intent(in)  :: block_sizes(:)

      ! Local variables
      integer                     :: rank
      character(len=8)            :: kind_string
      character(len=3)            :: accum_string
      character(len=16)           :: bufftype_string
      integer,          parameter :: max_rank = 2
      integer,          parameter :: good_kinds(2) = (/ REAL32, REAL64 /)
      character(len=*), parameter :: subname = 'hist_new_buffer'
      character(len=*), parameter :: errhead = subname//' ERROR: '

      ! Initialize output and local variables
      nullify(buffer)
      if (present(errmsg)) then
         errmsg = ''
      end if
      rank = SIZE(buff_shape, 1)
      !! Some sanity checks
      ! Check kind
      if (.not. ANY(good_kinds == buff_kind)) then
         write(errmsg(len_trim(errmsg)+1:), '(2a,i0,a)') errhead,             &
              'buff_type, ', buff_kind, ' not supported'
      end if
      select case (buff_kind)
      case (REAL32)
         kind_string = 'real32'
      case (REAL64)
         kind_string = 'real64'
      case (INT32)
         kind_string = 'int32'
      case (INT64)
         kind_string = 'int64'
      case default
         kind_string = ''
      end select
      ! Check horiz_axis_ind
      if ((horiz_axis_ind < 1) .or. (horiz_axis_ind > rank)) then
         write(errmsg(len_trim(errmsg)+1:), '(2a,i0,a)') errhead,             &
              'horiz_axis_ind outside of valid range, [1, ', rank, ']'
      end if
      ! Check for (proper) block structured buffer
      if (present(block_ind) .and. present(block_sizes)) then
         if ((block_ind < 1) .or. (block_ind > rank)) then
            write(errmsg(len_trim(errmsg)+1:), '(2a,i0,a)') errhead,          &
                 'block_ind outside of valid range, [1, ', rank, ']'
         else if (block_ind == horiz_axis_ind) then
            write(errmsg(len_trim(errmsg)+1:), '(2a)') errhead,               &
                 'block_ind cannot be the same as horiz_axis_ind'
         end if
      else if (present(block_ind)) then
         if (present(errmsg)) then
            write(errmsg(len_trim(errmsg)+1:), '(2a)') errhead,               &
                 'block_sizes required if block_ind is present'
         end if
      else if (present(block_sizes)) then
         if (present(errmsg)) then
            write(errmsg(len_trim(errmsg)+1:), '(2a)') errhead,               &
                 'block_ind required if block_sizes is present'
         end if
      end if ! No else, we just do not have a blocked buffer
      ! Check accumulation type
      select case(trim(accum_type))
      case ('I', 'i', 'lst')
         accum_string = 'lst'
      case ('A', 'a', 'avg')
         accum_string = 'avg'
      case ('M', 'm', 'min')
         accum_string = 'min'
      case ('X', 'x', 'max')
         accum_string = 'max'
      case ('S', 's', 'var')
         accum_string = 'var'
      case default
         if (present(errmsg)) then
            write(errmsg(len_trim(errmsg)+1:), '(4a)') errhead,               &
                 "Unknown accumulation operator type, '", trim(accum_type), "'"
         end if
      end select
      ! First, sort by rank
      select case (rank)
      case (1)
         ! sort by kind (already checked above)
         if (buff_kind == REAL32) then
            buffer => buffer_factory('real32_1_')
         else if (buff_kind == REAL64) then
            buffer => buffer_factory('real64_1_')
         end if
      case default
         ! Over max rank currently handled
         if (present(errmsg)) then
            write(errmsg(len_trim(errmsg)+1:), '(2a,i0)') errhead,            &
                 'buffers have a max rank of ', max_rank
         end if
      end select

   end subroutine hist_new_buffer

   !#######################################################################

   subroutine hist_buffer_accumulate_1dreal32(buffer, field)
      class(hist_buffer_1dreal32_inst_t), intent(inout) :: buffer
      real(REAL32),                       intent(in)    :: field(:)
   end subroutine hist_buffer_accumulate_1dreal32

   !#######################################################################

   subroutine hist_buffer_accumulate_1dreal64(buffer, field)
      class(hist_buffer_1dreal64_inst_t), intent(inout) :: buffer
      real(REAL64),                       intent(in)    :: field(:)
   end subroutine hist_buffer_accumulate_1dreal64

end module hist_api
