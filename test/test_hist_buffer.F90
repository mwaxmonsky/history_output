program test_hist_buffer
   use ISO_FORTRAN_ENV,  only: REAL64, REAL32, INT32, INT64
   use hist_msg_handler, only: hist_log_messages
   use hist_buffer,      only: hist_buffer_t
   use hist_field,       only: hist_field_info_t, hist_get_field
   use hist_api,         only: hist_new_field, hist_new_buffer

   class(hist_field_info_t), pointer :: my_fields => NULL()
   class(hist_buffer_t),     pointer :: buffer => NULL()
   class(hist_buffer_t),     pointer :: buff_ptr => NULL()
   type(hist_log_messages)           :: errors

   my_fields => hist_new_field('U', 'eastward_wind', 'Meridional Wind',       &
        'm s-1', 'real', errors=errors)
   call hist_new_buffer(my_fields, (/ 5 /), REAL32, 1, 'lst', 1, buffer,      &
        errors=errors)

   if (errors%num_errors() > 0) then
      write(6, '(a,i0,a)') 'FAIL, ', errors%num_errors(), ' errors found'
      call errors%output(6)
      flush(6)
      STOP 1
   else
      STOP 0
   end if

end program test_hist_buffer
