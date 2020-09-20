program test_hash
   use hist_hash_table, only: hist_hashable_t, hist_hashable_char_t
   use hist_hash_table, only: hist_hash_table_t

   type(hist_hashable_char_t), allocatable :: hash_chars(:)
   type(hist_hash_table_t)                 :: hash_table

   call hash_table%initialize(10, 5)

end program test_hash
