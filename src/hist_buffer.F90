module hist_buffer
   use ISO_FORTRAN_ENV, only: REAL64, REAL32, INT32, INT64
   use hist_hashable,   only: hist_hashable_t

   implicit none
   private

   ! Public interfaces
   public :: buffer_factory

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

   ! Abstract interface for clear procedure of hist_buffer_t class
   abstract interface
      subroutine hist_buff_clear(this)
         import :: hist_buffer_t
         class(hist_buffer_t), intent(inout) :: this
      end subroutine hist_buff_clear
   end interface

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

   subroutine buff_1dreal32_inst_accum(this, field, errmsg)
      class(hist_buffer_1dreal32_inst_t), intent(inout) :: this
      real(REAL32),                       intent(in)    :: field(:)
      character(len=*), optional,         intent(out)   :: errmsg

      if (present(errmsg)) then
         errmsg = 'Not implemented'
      end if
      this%num_samples = 1

   end subroutine buff_1dreal32_inst_accum

   !#######################################################################

   subroutine buff_1dreal32_inst_value(this, norm_val, errmsg)
      class(hist_buffer_1dreal32_inst_t), intent(inout) :: this
      real(REAL32),                       intent(inout) :: norm_val(:)
      character(len=*), optional,         intent(out)   :: errmsg

      if (present(errmsg)) then
         errmsg = 'Not implemented'
      end if

   end subroutine buff_1dreal32_inst_value

   !#######################################################################

   subroutine buff_1dreal64_inst_clear(this)
      class(hist_buffer_1dreal64_inst_t), intent(inout) :: this

      this%num_samples = 0

   end subroutine buff_1dreal64_inst_clear

   !#######################################################################

   subroutine buff_1dreal64_inst_accum(this, field, errmsg)
      class(hist_buffer_1dreal64_inst_t), intent(inout) :: this
      real(REAL64),                       intent(in)    :: field(:)
      character(len=*), optional,         intent(out)   :: errmsg

      if (present(errmsg)) then
         errmsg = 'Not implemented'
      end if

   end subroutine buff_1dreal64_inst_accum

   !#######################################################################

   subroutine buff_1dreal64_inst_value(this, norm_val, errmsg)
      class(hist_buffer_1dreal64_inst_t), intent(inout) :: this
      real(REAL64),                       intent(inout) :: norm_val(:)
      character(len=*), optional,         intent(out)   :: errmsg

      if (present(errmsg)) then
         errmsg = 'Not implemented'
      end if

   end subroutine buff_1dreal64_inst_value

   !#######################################################################

   function buffer_factory(buffer_type) result(newbuf)
      ! Create a new buffer based on <buffer_type>.
      ! <buffer_type> has a format typekind_rank_accum
      ! Where:
      ! <typekind> is a lowercase string representation of a
      !    supported kind from the ISO_FORTRAN_ENV module.
      ! <rank> is the rank of the buffer (no leading zeros)
      ! <accum> is the accumulation type, one of:
      !    lst: Store the last value collected
      !    avg: Accumulate running average
      !    var: Accumulate standard deviation
      !    min: Accumulate smallest value
      !    max: Accumulate largest value
      ! Arguments
      class(hist_buffer_t), pointer :: newbuf
      character(len=*), intent(in)  :: buffer_type
      ! Local variables
      ! For buffer allocation
      type(hist_buffer_1dreal32_inst_t), pointer :: real32_1_in => NULL()
      type(hist_buffer_1dreal64_inst_t), pointer :: real64_1_in => NULL()


      ! Create new buffer
      select case (trim(buffer_type))
      case ('real32_1_inst')
         allocate(real32_1_in)
         newbuf => real32_1_in
      case ('real64_1_inst')
         allocate(real64_1_in)
         newbuf => real64_1_in
      end select
   end function buffer_factory

end module hist_buffer
