module hist_buffer
   use ISO_FORTRAN_ENV, only: REAL64, REAL32, INT32, INT64
   use hist_hashable,   only: hist_hashable_t

   implicit none
   private

   ! Public interfaces
   public :: buffer_factory

   ! Time sampling flag indices
   !!XXgoldyXX: Todo: decide on sampling types

   type, abstract, public :: hist_buffer_t
      ! hist_buffer_t is an abstract base class for hist_outfld buffers
      class(hist_hashable_t), pointer              :: field_info => NULL()
      integer,                             private :: vol = -1 ! For host output
      integer,                             private :: horiz_axis_ind = 0
      integer,                allocatable, private :: field_shape(:)
      integer,                allocatable, private :: num_samples(:)
      character(len=:),       allocatable, private :: accum_str
      class(hist_buffer_t),   pointer              :: next
   contains
      procedure                                 :: field  => get_field_info
      procedure                                 :: volume => get_volume
      procedure                                 :: horiz_axis_index
      procedure                                 :: init_buffer
      procedure                                 :: accum_string
      procedure(hist_buff_init),       deferred :: initialize
      procedure(hist_buff_sub_log), deferred :: clear
   end type hist_buffer_t

   type, public, extends(hist_buffer_t) :: hist_buff_1dreal32_lst_t
      real(REAL32), pointer :: data(:) => NULL()
   CONTAINS
      procedure :: clear => buff_1dreal32_lst_clear
      procedure :: accumulate => buff_1dreal32_lst_accum
      procedure :: norm_value => buff_1dreal32_lst_value
      procedure :: initialize => init_buff_1dreal32
   end type hist_buff_1dreal32_lst_t

   type, public, extends(hist_buff_1dreal32_lst_t) :: hist_buff_1dreal32_avg_t
   CONTAINS
      procedure :: accumulate => buff_1dreal32_avg_accum
      procedure :: norm_value => buff_1dreal32_avg_value
      procedure :: initialize => init_buff_avg_1dreal32
   end type hist_buff_1dreal32_avg_t

   type, public, extends(hist_buffer_t) :: hist_buff_1dreal64_lst_t
      real(REAL64), pointer :: data(:) => NULL()
   CONTAINS
      procedure :: clear => buff_1dreal64_lst_clear
      procedure :: accumulate => buff_1dreal64_lst_accum
      procedure :: norm_value => buff_1dreal64_lst_value
      procedure :: initialize => init_buff_1dreal64
   end type hist_buff_1dreal64_lst_t

   ! Abstract interfaces for hist_buffer_t class
   abstract interface
      subroutine hist_buff_sub_log(this, logger)
         use hist_msg_handler, only: hist_log_messages
         import                   :: hist_buffer_t
         class(hist_buffer_t),              intent(inout) :: this
         type(hist_log_messages), optional, intent(inout) :: logger
      end subroutine hist_buff_sub_log
   end interface

   abstract interface
      subroutine hist_buff_init(this, field_in, volume_in, horiz_axis_in,     &
           shape_in, block_sizes_in, block_ind_in, logger)
         use hist_msg_handler, only: hist_log_messages
         import                   :: hist_buffer_t
         import                   :: hist_hashable_t
         class(hist_buffer_t),              intent(inout) :: this
         class(hist_hashable_t),  pointer                 :: field_in
         integer,                           intent(in)    :: volume_in
         integer,                           intent(in)    :: horiz_axis_in
         integer,                           intent(in)    :: shape_in(:)
         integer,                 optional, intent(in)    :: block_sizes_in(:)
         integer,                 optional, intent(in)    :: block_ind_in
         type(hist_log_messages), optional, intent(inout) :: logger
      end subroutine hist_buff_init
   end interface

CONTAINS

   !#######################################################################

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

   integer function horiz_axis_index(this)
      class(hist_buffer_t), intent(in) :: this

      horiz_axis_index = this%horiz_axis_ind
   end function horiz_axis_index

   !#######################################################################

   subroutine init_buffer(this, field_in, volume_in, horiz_axis_in, shape_in, &
        block_sizes_in, block_ind_in, logger)
      use hist_msg_handler, only: hist_log_messages, hist_add_alloc_error

      ! Dummy arguments
      class(hist_buffer_t),              intent(inout) :: this
      class(hist_hashable_t),  pointer                 :: field_in
      integer,                           intent(in)    :: volume_in
      integer,                           intent(in)    :: horiz_axis_in
      integer,                           intent(in)    :: shape_in(:)
      integer,                 optional, intent(in)    :: block_sizes_in(:)
      integer,                 optional, intent(in)    :: block_ind_in
      type(hist_log_messages), optional, intent(inout) :: logger
      ! Local variables
      integer                     :: astat
      character(len=*), parameter :: subname = 'init_buffer'

      this%field_info => field_in
      this%vol = volume_in
      this%horiz_axis_ind = horiz_axis_in
      allocate(this%field_shape(size(shape_in, 1)), stat=astat)
      if (astat == 0) then
         this%field_shape(:) = shape_in(:)
      else
         call hist_add_alloc_error('field_shape', __FILE__, __LINE__ - 4,     &
              subname=subname, errors=logger)
      end if
   end subroutine init_buffer

   !#######################################################################

   function accum_string(this) result(ac_str)
      class(hist_buffer_t), intent(in) :: this
      character(len=:), allocatable    :: ac_str

      ac_str = this%accum_str
   end function accum_string

   !#######################################################################

   subroutine buff_1dreal32_lst_clear(this, logger)
      use hist_msg_handler, only: hist_log_messages, hist_add_alloc_error

      ! Dummy arguments
      class(hist_buff_1dreal32_lst_t),   intent(inout) :: this
      type(hist_log_messages), optional, intent(inout) :: logger
      ! Local variables
      integer                     :: aerr
      character(len=*), parameter :: subname = 'buff_1dreal32_lst_clear'

      this%num_samples = 0
      if (.not. associated(this%data)) then
         allocate(this%data(this%field_shape(1)), stat=aerr)
         if (aerr /= 0) then
            call hist_add_alloc_error('data', __FILE__, __LINE__ - 2,         &
                 subname=subname, errors=logger)
         end if
      end if
      this%data = 0.0_REAL32

   end subroutine buff_1dreal32_lst_clear

   !#######################################################################

   subroutine init_buff_1dreal32(this, field_in, volume_in, horiz_axis_in, &
        shape_in, block_sizes_in, block_ind_in, logger)
      use hist_msg_handler, only: hist_log_messages, hist_add_alloc_error

      ! Dummy arguments
      class(hist_buff_1dreal32_lst_t),   intent(inout) :: this
      class(hist_hashable_t),  pointer                 :: field_in
      integer,                           intent(in)    :: volume_in
      integer,                           intent(in)    :: horiz_axis_in
      integer,                           intent(in)    :: shape_in(:)
      integer,                 optional, intent(in)    :: block_sizes_in(:)
      integer,                 optional, intent(in)    :: block_ind_in
      type(hist_log_messages), optional, intent(inout) :: logger

      call init_buffer(this, field_in, volume_in, horiz_axis_in, shape_in,    &
           block_sizes_in, block_ind_in, logger=logger)
      call this%clear(logger=logger)
      this%accum_str = 'last sampled value'

   end subroutine init_buff_1dreal32

   !#######################################################################

   subroutine buff_1dreal32_lst_accum(this, field, errmsg)
      class(hist_buff_1dreal32_lst_t), intent(inout) :: this
      real(REAL32),                       intent(in)    :: field(:)
      character(len=*), optional,         intent(out)   :: errmsg

      if (present(errmsg)) then
         errmsg = 'Not implemented'
      end if
      this%num_samples = 1

   end subroutine buff_1dreal32_lst_accum

   !#######################################################################

   subroutine buff_1dreal32_lst_value(this, norm_val, errmsg)
      class(hist_buff_1dreal32_lst_t), intent(inout) :: this
      real(REAL32),                    intent(inout) :: norm_val(:)
      character(len=*), optional,      intent(out)   :: errmsg

      if (present(errmsg)) then
         errmsg = 'Not implemented'
      end if

   end subroutine buff_1dreal32_lst_value

   !#######################################################################

   subroutine init_buff_avg_1dreal32(this, field_in, volume_in,               &
        horiz_axis_in, shape_in, block_sizes_in, block_ind_in, logger)
      use hist_msg_handler, only: hist_log_messages, hist_add_alloc_error

      ! Dummy arguments
      class(hist_buff_1dreal32_avg_t),   intent(inout) :: this
      class(hist_hashable_t),  pointer                 :: field_in
      integer,                           intent(in)    :: volume_in
      integer,                           intent(in)    :: horiz_axis_in
      integer,                           intent(in)    :: shape_in(:)
      integer,                 optional, intent(in)    :: block_sizes_in(:)
      integer,                 optional, intent(in)    :: block_ind_in
      type(hist_log_messages), optional, intent(inout) :: logger

      call init_buffer(this, field_in, volume_in, horiz_axis_in, shape_in,    &
           block_sizes_in, block_ind_in, logger=logger)
      call this%clear(logger=logger)
      this%accum_str = 'average of sampled values'

   end subroutine init_buff_avg_1dreal32

   !#######################################################################

   subroutine buff_1dreal32_avg_accum(this, field, errmsg)
      class(hist_buff_1dreal32_avg_t), intent(inout) :: this
      real(REAL32),                    intent(in)    :: field(:)
      character(len=*), optional,      intent(out)   :: errmsg

      if (present(errmsg)) then
         errmsg = 'Not implemented'
      end if
      this%num_samples = 1

   end subroutine buff_1dreal32_avg_accum

   !#######################################################################

   subroutine buff_1dreal32_avg_value(this, norm_val, errmsg)
      class(hist_buff_1dreal32_avg_t), intent(inout) :: this
      real(REAL32),                    intent(inout) :: norm_val(:)
      character(len=*), optional,      intent(out)   :: errmsg

      if (present(errmsg)) then
         errmsg = 'Not implemented'
      end if

   end subroutine buff_1dreal32_avg_value

   !#######################################################################

   subroutine buff_1dreal64_lst_clear(this, logger)
      use hist_msg_handler, only: hist_log_messages, hist_add_alloc_error

      ! Dummy arguments
      class(hist_buff_1dreal64_lst_t),   intent(inout) :: this
      type(hist_log_messages), optional, intent(inout) :: logger
      ! Local variables
      integer                     :: aerr
      character(len=*), parameter :: subname = 'buff_1dreal64_lst_clear'

      this%num_samples = 0
      if (.not. associated(this%data)) then
         allocate(this%data(this%field_shape(1)), stat=aerr)
         if (aerr /= 0) then
            call hist_add_alloc_error('data', __FILE__, __LINE__ - 1,         &
                 subname=subname, errors=logger)
         end if
      end if
      this%data = 0.0_REAL64

   end subroutine buff_1dreal64_lst_clear

   !#######################################################################

   subroutine init_buff_1dreal64(this, field_in, volume_in, horiz_axis_in,    &
        shape_in, block_sizes_in, block_ind_in, logger)
      use hist_msg_handler, only: hist_log_messages

      class(hist_buff_1dreal64_lst_t),   intent(inout) :: this
      class(hist_hashable_t),  pointer                 :: field_in
      integer,                           intent(in)    :: volume_in
      integer,                           intent(in)    :: horiz_axis_in
      integer,                           intent(in)    :: shape_in(:)
      integer,                 optional, intent(in)    :: block_sizes_in(:)
      integer,                 optional, intent(in)    :: block_ind_in
      type(hist_log_messages), optional, intent(inout) :: logger

      call init_buffer(this, field_in, volume_in, horiz_axis_in, shape_in,    &
           block_sizes_in, block_ind_in, logger=logger)
      call this%clear(logger=logger)
      this%accum_str = 'last sampled value'

   end subroutine init_buff_1dreal64

   !#######################################################################

   subroutine buff_1dreal64_lst_accum(this, field, errmsg)
      class(hist_buff_1dreal64_lst_t), intent(inout) :: this
      real(REAL64),                    intent(in)    :: field(:)
      character(len=*), optional,      intent(out)   :: errmsg

      if (present(errmsg)) then
         errmsg = 'Not implemented'
      end if

   end subroutine buff_1dreal64_lst_accum

   !#######################################################################

   subroutine buff_1dreal64_lst_value(this, norm_val, errmsg)
      class(hist_buff_1dreal64_lst_t), intent(inout) :: this
      real(REAL64),                    intent(inout) :: norm_val(:)
      character(len=*), optional,      intent(out)   :: errmsg

      if (present(errmsg)) then
         errmsg = 'Not implemented'
      end if

   end subroutine buff_1dreal64_lst_value

   !#######################################################################

   function buffer_factory(buffer_type, logger) result(newbuf)
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

      use hist_msg_handler, only: hist_log_messages, ERROR, VERBOSE
      use hist_msg_handler, only: hist_add_error

      ! Arguments
      class(hist_buffer_t),    pointer                 :: newbuf
      character(len=*),                  intent(in)    :: buffer_type
      type(hist_log_messages), optional, intent(inout) :: logger

      ! Local variables
      character(len=*),               parameter :: subname = 'buffer_factory'
      ! For buffer allocation
      integer                                   :: aerr
      type(hist_buff_1dreal32_lst_t), pointer   :: real32_1_in => NULL()
      type(hist_buff_1dreal64_lst_t), pointer   :: real64_1_in => NULL()


      nullify(newbuf)
      ! Create new buffer
      select case (trim(buffer_type))
      case ('real32_1_lst')
         allocate(real32_1_in, stat=aerr)
         if (aerr == 0) then
            newbuf => real32_1_in
         end if
      case ('real64_1_lst')
         allocate(real64_1_in, stat=aerr)
         if (aerr == 0) then
            newbuf => real64_1_in
         end if
      case default
         call hist_add_error(subname,                                         &
              "Invalid or unsupported buffer type, '",                        &
              errstr2=trim(buffer_type), errstr3="'", errors=logger)
      end select
   end function buffer_factory

end module hist_buffer
