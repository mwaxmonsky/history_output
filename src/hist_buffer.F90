module hist_buffer

   use hist_hashable, only: hist_hashable_t

   implicit none
   private

   ! Processing flag indices
   integer, parameter :: hist_proc_last    = 1 ! Save last sample
   integer, parameter :: hist_proc_average = 2 ! Average samples
   integer, parameter :: hist_proc_stddev  = 3 ! Standard deviation of samples
   integer, parameter :: hist_proc_min     = 4 ! Minimum of samples
   integer, parameter :: hist_proc_max     = 5 ! Maximum of samples
   ! Time sampling flag indices
   !!XXgoldyXX: Todo: decide on sampling types

   type, abstract, public :: hist_buffer_t
      ! hist_buffer_t is an abstract base class for hist_outfld buffers
      class(hist_hashable_t), pointer, private :: field_info => NULL()
      integer,                         private :: vol = -1 ! For host output
      class(hist_buffer_t),   pointer          :: next
   contains
      procedure :: field  => get_field_info
      procedure :: volume => get_volume
      procedure :: clear  => clear_buffer
   end type hist_buffer_t

! Accumulation count?
! Accumulation method
! Normalized value method

   type, public, extends(hist_buffer_t) :: hist_buffer_1dreal_inst_t(bkind)
      kind                 :: bkind
      real(bkind), pointer :: data(:)
   end type hist_buffer_1dreal_inst_t

   type, public, extends(hist_buffer_t) :: hist_buffer_1dreal_avg_t(bkind)
      kind                 :: bkind
      real(bkind), pointer :: avg(:) ! Running average
   end type hist_buffer_1dreal_avg_t

   type, public, extends(hist_buffer_t) :: hist_buffer_1dreal_var_t(bkind)
      kind                 :: bkind
      real(bkind), pointer :: avg(:) ! Running average
      real(bkind), pointer :: var(:) ! Running 'variance^2'
   end type hist_buffer_1dreal_var_t

CONTAINS

   function get_field_info(this)
      class(hist_buffer_t), intent(in) :: this
      class(hist_hashable_t), pointer  :: get_field_info

      get_field_info => this%field_info
   end function get_field_info

   !#######################################################################

   integer function get_volume(this)
      class(hist_buffer_t), intent(in) :: this

      get_volume = this%vol
   end function get_volume

   !#######################################################################

end module hist_buffer
