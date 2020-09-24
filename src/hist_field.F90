module hist_field
   ! Module containing DDTs for history fields and associated routines

   use hist_hashable, only: hist_hashable_t
   use hist_buffer,   only: hist_buffer_t

   implicit none
   private

   public :: hist_new_field ! Allocate a hist_field_info_t object
   public :: hist_get_field ! Get field from a buffer

   type, public, extends(hist_hashable_t) :: hist_field_info_t
      ! Field metadata
      character(len=:), allocatable, private :: diag_name ! History file name
      character(len=:), allocatable, private :: standard_name
      character(len=:), allocatable, private :: long_name
      character(len=:), allocatable, private :: units
! type kind rank?
      ! dimensions?
      type(hist_field_info_t), pointer :: next => NULL()
      class(hist_buffer_t),    pointer :: buffers => NULL()
   contains
      procedure :: key => hist_field_info_get_key
      final     :: finalize_field
   end type hist_field_info_t

CONTAINS

   !#######################################################################

   function hist_field_info_get_key(hashable)
      ! Return the hashable field info class key (diag_name)
      class(hist_field_info_t), intent(in) :: hashable
      character(len=:), allocatable        :: hist_field_info_get_key

      hist_field_info_get_key = hashable%diag_name
   end function hist_field_info_get_key

   !#######################################################################

   subroutine hist_new_field(new_field, diag_name_in, std_name_in,            &
        long_name_in, units_in)
      type(hist_field_info_t), pointer :: new_field
      character(len=*), intent(in)     :: diag_name_in
      character(len=*), intent(in)     :: std_name_in
      character(len=*), intent(in)     :: long_name_in
      character(len=*), intent(in)     :: units_in

      if (associated(new_field)) then
         deallocate(new_field)
      end if
      allocate(new_field)
      new_field%diag_name = diag_name_in
      new_field%standard_name = std_name_in
      new_field%long_name = long_name_in
      new_field%units = units_in
   end subroutine hist_new_field

   !#######################################################################

   subroutine hist_get_field(buffer, field)
      ! Retrieve
      class(hist_buffer_t), intent(in) :: buffer
      type(hist_field_info_t), pointer :: field

      select type(finfo => buffer%field_info)
         type is (hist_field_info_t)
            field => finfo
         end select
   end subroutine hist_get_field

   !#######################################################################

   subroutine finalize_field(this)
      ! Dummy Argument
      type(hist_field_info_t) :: this
      ! Local Variables
      class(hist_buffer_t), pointer :: next_buf

      if (allocated(this%diag_name)) then
         deallocate(this%diag_name)
      end if
      if (allocated(this%standard_name)) then
         deallocate(this%standard_name)
      end if
      if (allocated(this%long_name)) then
         deallocate(this%long_name)
      end if
      if (allocated(this%units)) then
         deallocate(this%units)
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
