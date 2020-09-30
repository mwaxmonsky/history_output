module hist_field
   ! Module containing DDTs for history fields and associated routines

   use hist_hashable, only: hist_hashable_t
   use hist_buffer,   only: hist_buffer_t

   implicit none
   private

   public :: hist_field_initialize
   public :: hist_get_field ! Get field from a buffer

   type, public, extends(hist_hashable_t) :: hist_field_info_t
      ! Field metadata (note: all strings should be lowercase)
      character(len=:), allocatable, private :: diag_file_name
      character(len=:), allocatable, private :: field_standard_name
      character(len=:), allocatable, private :: field_long_name
      character(len=:), allocatable, private :: field_units
      character(len=:), allocatable, private :: field_type
! type kind rank?
      ! dimensions?
      type(hist_field_info_t), pointer :: next => NULL()
      class(hist_buffer_t),    pointer :: buffers => NULL()
   contains
      procedure :: key           => hist_field_info_get_key
      procedure :: diag_name     => field_get_diag_name
      procedure :: standard_name => field_get_standard_name
      procedure :: long_name     => field_get_long_name
      procedure :: units         => field_get_units
      procedure :: type          => field_get_type
      final     :: finalize_field
   end type hist_field_info_t

CONTAINS

   !#######################################################################

   function hist_field_info_get_key(hashable)
      ! Return the hashable field info class key (diag_file_name)
      class(hist_field_info_t), intent(in) :: hashable
      character(len=:), allocatable        :: hist_field_info_get_key

      hist_field_info_get_key = hashable%diag_file_name
   end function hist_field_info_get_key

   !#######################################################################

   subroutine hist_field_initialize(field, diag_name_in, std_name_in,         &
        long_name_in, units_in, type_in, errmsg)

      type(hist_field_info_t), pointer               :: field
      character(len=*),                  intent(in)  :: diag_name_in
      character(len=*),                  intent(in)  :: std_name_in
      character(len=*),                  intent(in)  :: long_name_in
      character(len=*),                  intent(in)  :: units_in
      character(len=*),                  intent(in)  :: type_in
      character(len=*),        optional, intent(out) :: errmsg

      if (present(errmsg)) then
         errmsg = ''
      end if
      field%diag_file_name = diag_name_in
      field%field_standard_name = std_name_in
      field%field_long_name = long_name_in
      field%field_units = units_in
      field%field_type = type_in
   end subroutine hist_field_initialize

   !#######################################################################

   subroutine hist_get_field(buffer, field)
      ! Retrieve
      class(hist_buffer_t), intent(in) :: buffer
      type(hist_field_info_t), pointer :: field

      nullify(field)
      select type(finfo => buffer%field_info)
      type is (hist_field_info_t)
         field => finfo
      end select
   end subroutine hist_get_field

   !#######################################################################

   function field_get_diag_name(this) result(info)
      class(hist_field_info_t), intent(in) :: this
      character(len=:), allocatable        :: info

      info = this%diag_file_name
   end function field_get_diag_name

   !#######################################################################

   function field_get_standard_name(this) result(info)
      class(hist_field_info_t), intent(in) :: this
      character(len=:), allocatable        :: info

      info = this%field_standard_name
   end function field_get_standard_name

   !#######################################################################

   function field_get_long_name(this) result(info)
      class(hist_field_info_t), intent(in) :: this
      character(len=:), allocatable        :: info

      info = this%field_long_name
   end function field_get_long_name

   !#######################################################################

   function field_get_units(this) result(info)
      class(hist_field_info_t), intent(in) :: this
      character(len=:), allocatable        :: info

      info = this%field_units
   end function field_get_units

   !#######################################################################

   function field_get_type(this) result(info)
      class(hist_field_info_t), intent(in) :: this
      character(len=:), allocatable        :: info

      info = this%field_type
   end function field_get_type

   !#######################################################################

   subroutine finalize_field(this)
      ! Dummy Argument
      type(hist_field_info_t) :: this
      ! Local Variables
      class(hist_buffer_t), pointer :: next_buf

      if (allocated(this%diag_file_name)) then
         deallocate(this%diag_file_name)
      end if
      if (allocated(this%field_standard_name)) then
         deallocate(this%field_standard_name)
      end if
      if (allocated(this%field_long_name)) then
         deallocate(this%field_long_name)
      end if
      if (allocated(this%field_units)) then
         deallocate(this%field_units)
      end if
      if (allocated(this%field_type)) then
         deallocate(this%field_type)
      end if
      ! We are not in charge of the field chain so just nullify
      nullify(this%next)
      ! We are in charge of the buffers so get rid of them.
      do
         next_buf => this%buffers
         if (associated(next_buf)) then
            this%buffers => next_buf%next
            deallocate(next_buf)
         else
            exit
         end if
      end do
   end subroutine finalize_field

end module hist_field
