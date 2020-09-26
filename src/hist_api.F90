module hist_api

   use ISO_FORTRAN_ENV, only: REAL64, REAL32, INT32, INT64
   use hist_utils,      only: hist_error_messages
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

   logical function have_error(errors)
      ! Return .true. iff <errors> is present and contains error messages
      type(hist_error_messages), optional, intent(inout) :: errors

      have_error = present(errors)
      if (have_error) then
         have_error = errors%num_errors() > 0
      end if
   end function have_error

   !#######################################################################

   function hist_new_field(diag_name_in, std_name_in, long_name_in, units_in, &
        type_in, errors) result(new_field)
      use hist_field, only: hist_field_initialize

      type(hist_field_info_t),   pointer                 :: new_field
      character(len=*),                    intent(in)    :: diag_name_in
      character(len=*),                    intent(in)    :: std_name_in
      character(len=*),                    intent(in)    :: long_name_in
      character(len=*),                    intent(in)    :: units_in
      character(len=*),                    intent(in)    :: type_in
      type(hist_error_messages), optional, intent(inout) :: errors

      integer                     :: astat
      character(len=128)          :: errmsg
      character(len=*), parameter :: subname = 'hist_new_field'

      if (.not. have_error(errors)) then
         allocate(new_field, stat=astat)
         if ((astat /= 0) .and. present(errors)) then
            call errors%new_error(subname//' Unable to allocate <new_field>')
         end if
      end if
      if (.not. have_error(errors)) then
         call hist_field_initialize(new_field, diag_name_in, std_name_in,     &
              long_name_in, units_in, type_in, errmsg)
         if (len_trim(errmsg) > 0) then
            call errors%new_error(', called from '//subname)
         end if
      end if
   end function hist_new_field

   !#######################################################################

   subroutine hist_new_buffer(field, buff_shape, buff_kind, horiz_axis_ind,   &
        accum_type, output_vol, buffer, errors, block_ind, block_sizes)
      ! Dummy arguments
      class(hist_field_info_t),  pointer                 :: field
      integer,                             intent(in)    :: buff_shape(:)
      integer,                             intent(in)    :: buff_kind
      integer,                             intent(in)    :: horiz_axis_ind
      character(len=*),                    intent(in)    :: accum_type
      integer,                             intent(in)    :: output_vol
      class(hist_buffer_t),      pointer,  intent(out)   :: buffer
      type(hist_error_messages), optional, intent(inout) :: errors
      integer,                   optional, intent(in)    :: block_ind
      integer,                   optional, intent(in)    :: block_sizes(:)

      ! Local variables
      integer                              :: rank
      character(len=8)                     :: kind_string
      character(len=3)                     :: accum_string
      character(len=16)                    :: bufftype_string
      class(hist_hashable_t),  pointer     :: field_base
      class(hist_buffer_t),    pointer     :: buff_ptr
      character(len=:),        allocatable :: type_str
      integer,                 parameter   :: max_rank = 2
      character(len=*),        parameter   :: subname = 'hist_new_buffer'
      character(len=*),        parameter   :: errhead = subname//' ERROR: '

      ! Initialize output and local variables
      nullify(buffer)
      nullify(field_base)
      nullify(buff_ptr)
      rank = SIZE(buff_shape, 1)
      !! Some sanity checks
      ! We can select on the field's type string but not its kind string
      ! because we do not know the kind value for the kind string
      if (associated(field)) then
         type_str = field%type()
      else
         type_str = 'unknown'
      end if
      select case (type_str)
      case ('integer')
         select case (buff_kind)
         case (INT32)
            kind_string = 'int32'
         case (INT64)
            kind_string = 'int64'
         case default
            kind_string = ''
         end select
      case ('real')
         select case(buff_kind)
         case (REAL32)
            kind_string = 'real32'
         case (REAL64)
            kind_string = 'real64'
         case default
            kind_string = ''
         end select
      case default
         kind_string = ''
         if (present(errors)) then
            call errors%new_error(errhead//"type, '"//type_str,               &
                 errstr2="' is not supported")
         end if
      end select
      if ((len_trim(kind_string) == 0) .and. present(errors)) then
         call errors%new_error(errhead//"kind = ", errint1=buff_kind,         &
              errstr2=" is not supported for type "//type_str)
      end if
      ! Check horiz_axis_ind
      if ((horiz_axis_ind < 1) .or. (horiz_axis_ind > rank)) then
         call errors%new_error(errhead//'horiz_axis_ind outside of ',         &
              errstr2='valid range, [1, ', errint2=rank, errstr3=']')
      end if
      ! Check for (proper) block structured buffer
      if (present(block_ind) .and. present(block_sizes)) then
         if ((block_ind < 1) .or. (block_ind > rank)) then
            call errors%new_error(errhead//'block_ind outside of ',           &
                 errstr2='valid range, [1, ', errint2=rank, errstr3=']')
         else if (block_ind == horiz_axis_ind) then
            call errors%new_error(errhead//'block_ind cannot be the same ',   &
                 errstr2='as horiz_axis_ind')
         end if
      else if (present(block_ind)) then
         if (present(errors)) then
            call errors%new_error(errhead,                                    &
                 errstr2='block_sizes required if block_ind is present')
         end if
      else if (present(block_sizes)) then
         if (present(errors)) then
            call errors%new_error(errhead,                                   &
                 errstr2='block_ind required if block_sizes is present')
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
         if (present(errors)) then
            call errors%new_error(errhead//"Unknown accumulation operator",   &
                 errstr2=" type, '"//trim(accum_type)//"'")
         end if
      end select
      ! We now know what sort of buffer we need
      ! First, sort by rank
      select case (rank)
      case (1)
         ! sort by kind (already checked above)
         if (buff_kind == REAL32) then
            bufftype_string = 'real32_1_'//trim(accum_string)
         else if (buff_kind == REAL64) then
            bufftype_string = 'real64_1_'//trim(accum_string)
         end if
      case default
         ! Over max rank currently handled
         if (present(errors)) then
            call errors%new_error(errhead//'buffers have a max rank of ',     &
                 errint1=max_rank)
         end if
      end select
      buffer => buffer_factory(trim(bufftype_string))
      if (associated(buffer)) then
         field_base => field
         call buffer%initialize(field_base, output_vol, horiz_axis_ind,       &
              buff_shape, block_sizes, block_ind)
         ! Add this buffer to its field (field should be there if buffer is)
         if (associated(field%buffers)) then
            buff_ptr => field%buffers
            field%buffers => buffer
            buffer%next => buff_ptr
         else
            field%buffers => buffer
         end if
      else if (present(errors)) then
         call errors%new_error(errhead//'buffer ('//trim(bufftype_string),    &
              errstr2=') not created')
      end if

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
