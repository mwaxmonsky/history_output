module hist_buffer

   use ISO_FORTRAN_ENV, only: REAL64, REAL32
   use hist_hashable,   only: hist_hashable_t

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
      class(hist_hashable_t), pointer          :: field_info => NULL()
      integer,                         private :: vol = -1 ! For host output
      class(hist_buffer_t),   pointer          :: next
   contains
      procedure                            :: field  => get_field_info
      procedure                            :: volume => get_volume
      procedure(hist_buff_clear), deferred :: clear
   end type hist_buffer_t

   type, public, extends(hist_buffer_t) :: hist_buffer_1dreal32_inst_t
      integer               :: num_samples = 0
      real(REAL32), pointer :: data(:)
   CONTAINS
      procedure :: clear => buff_1dreal32_inst_clear
      procedure :: accumulate => buff_1dreal32_inst_accum
      procedure :: norm_value => buff_1dreal32_inst_value
   end type hist_buffer_1dreal32_inst_t

   type, public, extends(hist_buffer_t) :: hist_buffer_1dreal64_inst_t
      integer               :: num_samples = 0
      real(REAL64), pointer :: data(:)
   CONTAINS
      procedure :: clear => buff_1dreal64_inst_clear
      procedure :: accumulate => buff_1dreal64_inst_accum
      procedure :: norm_value => buff_1dreal64_inst_value
   end type hist_buffer_1dreal64_inst_t

!!XXgoldyXX: v debug only
#if 0
!!XXgoldyXX: ^ debug only
   type, public, extends(hist_buffer_t) :: hist_buffer_1dreal_avg_t(bkind)
      integer, kind        :: bkind
      integer              :: num_samples = 0
      real(bkind), pointer :: avg(:) => NULL() ! Running average
   CONTAINS
      procedure(hist_buff_clear) :: clear      => buff_1dreal_avg_clear
      procedure(hist_buff_accum) :: accumulate => buff_1dreal_avg_accum
      procedure(hist_buff_value) :: norm_value => buff_1dreal_avg_value
   end type hist_buffer_1dreal_avg_t

   type, public, extends(hist_buffer_t) :: hist_buffer_1dreal_var_t(bkind)
      integer, kind        :: bkind
      integer              :: num_samples = 0
      real(bkind), pointer :: avg(:) => NULL() ! Running average
      real(bkind), pointer :: var(:) => NULL() ! Running 'variance^2'
   CONTAINS
      procedure(hist_buff_clear) :: clear      => buff_1dreal_var_clear
      procedure(hist_buff_accum) :: accumulate => buff_1dreal_var_accum
      procedure(hist_buff_value) :: norm_value => buff_1dreal_var_value
   end type hist_buffer_1dreal_var_t
!!XXgoldyXX: v debug only
#endif
!!XXgoldyXX: ^ debug only

   ! Abstract interface for clear procedure of hist_buffer_t class
   abstract interface
      subroutine hist_buff_clear(this)
         import :: hist_buffer_t
         class(hist_buffer_t), intent(inout) :: this
      end subroutine hist_buff_clear
   end interface

!!XXgoldyXX: v debug only
#if 0
!!XXgoldyXX: ^ debug only
   ! Abstract interface for accumulate procedure of hist_buffer_t class
   abstract interface
      logical function hist_buff_accum_1dreal(this, field, errmsg)
         import :: hist_buffer_1dreal_t

         class(hist_buffer_1dreal_t(bkind)), intent(inout) :: this
         real(bkind),                        intent(in)    :: field(:)
         character(len=*), optional,         intent(out)   :: errmsg
      end function hist_buff_accum_1dreal
   end interface

   ! Abstract interface for norm_value procedure of hist_buffer_t class
   abstract interface
      subroutine hist_buff_value_1dreal(buffer, norm_val, errmsg)
         import :: hist_buffer_1dreal_t

         class(hist_buffer_1dreal_t(bkind)), intent(inout) :: buffer
         real(bkind),                        intent(inout) :: field(:)
         character(len=*), optional,         intent(out)   :: errmsg
      end subroutine hist_buff_value
   end interface
!!XXgoldyXX: v debug only
#endif
!!XXgoldyXX: ^ debug only

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

   subroutine buff_1dreal32_inst_clear(this)
      class(hist_buffer_1dreal32_inst_t), intent(inout) :: this

      this%num_samples = 0
   end subroutine buff_1dreal32_inst_clear

   !#######################################################################

   subroutine buff_1dreal64_inst_clear(this)
      class(hist_buffer_1dreal64_inst_t), intent(inout) :: this

      this%num_samples = 0
   end subroutine buff_1dreal64_inst_clear

   !#######################################################################

   logical function buff_1dreal32_inst_accum(this, errmsg) result(accum_ok)
      class(hist_buffer_1dreal32_inst_t), intent(inout) :: this
      character(len=*), optional,         intent(out)   :: errmsg

      errmsg = 'Not implemented'
      accum_ok = .false.
   end function buff_1dreal32_inst_accum

   !#######################################################################

   logical function buff_1dreal64_inst_accum(this, errmsg) result(accum_ok)
      class(hist_buffer_1dreal64_inst_t), intent(inout) :: this
      character(len=*), optional,         intent(out)   :: errmsg

      errmsg = 'Not implemented'
      accum_ok = .false.
   end function buff_1dreal64_inst_accum

   !#######################################################################

   subroutine buff_1dreal32_inst_value(this, norm_val, errmsg)
      class(hist_buffer_1dreal32_inst_t), intent(inout) :: this
      real(REAL32),                       intent(inout) :: norm_val(:)
      character(len=*), optional,         intent(out)   :: errmsg

      errmsg = 'Not implemented'
   end subroutine buff_1dreal32_inst_value

   !#######################################################################

   subroutine buff_1dreal64_inst_value(this, norm_val, errmsg)
      class(hist_buffer_1dreal64_inst_t), intent(inout) :: this
      real(REAL64),                       intent(inout) :: norm_val(:)
      character(len=*), optional,         intent(out)   :: errmsg

      errmsg = 'Not implemented'
   end subroutine buff_1dreal64_inst_value

end module hist_buffer
