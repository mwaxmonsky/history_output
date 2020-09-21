module test_hash_utils
   use hist_hash_table, only: hist_hashable_t, hist_hashable_char_t
   use hist_hash_table, only: new_hashable_char

   implicit none
   private

   public :: new_hash_char_array

CONTAINS

   subroutine new_hash_char_array(keys, hash_arr)
      character(len=*),                        intent(in)  :: keys(:)
      type(hist_hashable_char_t), allocatable, intent(out) :: hash_arr(:)

      integer :: index

      allocate(hash_arr(size(keys, 1)))
      do index = 1, size(hash_arr, 1)
         hash_arr(index) = new_hashable_char(keys(index))
      end do
   end subroutine new_hash_char_array

end module test_hash_utils

program test_hash
   use hist_hash_table, only: hist_hashable_char_t, hist_hash_table_t
   use test_hash_utils, only: new_hash_char_array

   character(len=10) :: hash_names(4) = (/ 'foo       ', 'bar       ',        &
        'foobar    ', 'big daddy ' /)
   type(hist_hashable_char_t), allocatable :: hash_chars(:)
   type(hist_hash_table_t)                 :: hash_table
   integer                                 :: index
   character(len=1024)                     :: errors
   character(len=128)                      :: errmsg

   errors = ''
   call new_hash_char_array(hash_names, hash_chars)
   call hash_table%initialize(10)
   do index = 1, size(hash_chars, 1)
      call hash_table%add_hash_key(hash_chars(index))
   end do

end program test_hash
