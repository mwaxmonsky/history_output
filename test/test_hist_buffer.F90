#if 0
module test_hist_buffer_utils
   use hist_field,  only: hist_field_info_t
   use hist_buffer, only: hist_buffer_t

   implicit none
   private

   public :: get_field

CONTAINS

end module test_hist_buffer_utils
#endif

program test_hist_buffer
   use hist_field,             only: hist_field_info_t, hist_get_field
   use hist_api,               only: hist_new_field

   type(hist_field_info_t), pointer  :: my_fields => NULL()
   integer                           :: index
   integer                           :: errcnt = 0
   integer,                parameter :: max_errs = 8
   integer,                parameter :: err_size = 128
   character(len=err_size)           :: errors(max_errs)

   errors = ''
   call hist_new_field(my_fields, 'U', 'eastward_wind', 'Meridional Wind',    &
        'm s-1', 'real')

   if (errcnt > 0) then
      write(6, '(a,i0,a)') 'FAIL, ', errcnt, ' errors found'
      do index = 1, errcnt
         write(6, *) trim(errors(index))
      end do
      STOP 1
   else
      STOP 0
   end if

end program test_hist_buffer
