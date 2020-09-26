module hist_utils

   implicit none
   private

   type :: hist_error_entry
      character(len=:),       allocatable :: error_message
      type(hist_error_entry), pointer     :: next => NULL()
   contains
      final :: finalize_error_entry
   end type hist_error_entry

   type, public :: hist_error_messages
      integer,                private          :: length = 0
      logical,                private          :: alloc_error = .false.
      integer,                private          :: max_len = -1 ! no limit
      type(hist_error_entry), private, pointer :: error_messages => NULL()
      type(hist_error_entry), private, pointer :: tail => NULL()
   contains
      procedure :: num_errors => hist_utils_num_errors
      procedure :: new_error  => hist_utils_new_error
      procedure :: output     => hist_utils_print_errors
      final     :: finalize_hist_error_messages
   end type hist_error_messages

CONTAINS

   !#######################################################################

   subroutine finalize_error_entry(this)
      ! Dummy Argument
      type(hist_error_entry) :: this
      ! Local argument
      type(hist_error_entry), pointer :: next

      if (allocated(this%error_message)) then
         deallocate(this%error_message)
      end if

      next => this%next
      nullify(this%next)
      if (associated(next)) then
         deallocate(next)
      end if
   end subroutine finalize_error_entry

   !#######################################################################

   integer function hist_utils_num_errors(this)
      ! Dummy Argument
      class(hist_error_messages) :: this

      hist_utils_num_errors = this%length
   end function hist_utils_num_errors

   !#######################################################################

   subroutine hist_utils_new_error(this, errstr1, errint1, errstr2, errint2,  &
        errstr3)
      ! Dummy Arguments
      class(hist_error_messages)             :: this
      character(len=*),           intent(in) :: errstr1
      integer,          optional, intent(in) :: errint1
      character(len=*), optional, intent(in) :: errstr2
      integer,          optional, intent(in) :: errint2
      character(len=*), optional, intent(in) :: errstr3

      ! Local variables
      integer                             :: aerr
      type(hist_error_entry), pointer     :: new_error
      character(len=:),       allocatable :: temp_str
      character(len=8)                    :: intstr

      temp_str = trim(errstr1)
      if (present(errint1)) then
         write(intstr, '(i0)') errint1
         temp_str = trim(temp_str)//intstr
      end if
      if (present(errstr2)) then
         temp_str = trim(temp_str)//errstr2
      end if
      if (present(errint2)) then
         write(intstr, '(i0)') errint2
         temp_str = trim(temp_str)//intstr
      end if
      if (present(errstr3)) then
         temp_str = trim(temp_str)//errstr3
      end if
      allocate(new_error, stat=aerr)
      ! We can't really report an error here so just trap and record
      if (aerr == 0) then
         new_error%error_message = trim(temp_str)
         if (.not. associated(this%error_messages)) then
            this%error_messages => new_error
         end if
         if (associated(this%tail)) then
            this%tail%next => new_error
         end if
         this%tail => new_error
         this%length = this%length + 1
      else
         this%alloc_error = .true.
      end if
   end subroutine hist_utils_new_error

   !#######################################################################

   subroutine hist_utils_print_errors(this, unit, header)
      ! Dummy Arguments
      class(hist_error_messages)             :: this
      integer,                    intent(in) :: unit
      character(len=*), optional, intent(in) :: header

      type(hist_error_entry), pointer     :: err_ptr

      err_ptr => this%error_messages
      if (present(header) .and. (this%num_errors() > 0)) then
         write(unit, '(a)') trim(header)
      end if
      if (this%alloc_error) then
         write(unit, '(a)') 'ERROR: Allocation error in error-handling system'
      end if
      do
         if (associated(err_ptr)) then
            write(unit, '(a)') trim(err_ptr%error_message)
            err_ptr => err_ptr%next
         else
            exit
         end if
      end do
   end subroutine hist_utils_print_errors

   !#######################################################################

   subroutine finalize_hist_error_messages(this)
      ! Dummy Argument
      type(hist_error_messages) :: this

      if (associated(this%error_messages)) then
         deallocate(this%error_messages)
      end if
      nullify(this%error_messages)
      nullify(this%tail)
      this%length = 0
   end subroutine finalize_hist_error_messages

end module hist_utils
