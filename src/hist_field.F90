module hist_field
   ! Module containing DDTs for history fields and associated routines

   implicit none
   private

   type, public :: hist_field_info_t
      ! Field metadata
      character(len=:), private :: diagnostic_name = '' ! Name on history file
      character(len=:), private :: standard_name = ''
      character(len=:), private :: long_name = ''
      character(len=:), private :: units = ''
! type kind rank?
      ! dimensions?
      type(hist_field_info_t), private :: next => NULL()
   end type hist_field_info_t

CONTAINS

end module hist_field
