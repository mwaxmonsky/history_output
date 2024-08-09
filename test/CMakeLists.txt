find_package(PFUNIT REQUIRED 
    #PATHS "/glade/u/home/mwaxmonsky/source/pFUnit/build/installed"
)

add_pfunit_ctest(hashable_test
  TEST_SOURCES test_hashable.pf
  LINK_LIBRARIES history
)

add_pfunit_ctest(hash_table_test
  TEST_SOURCES test_hash_table.pf
  LINK_LIBRARIES history
)

# target_include_directories(hashable_test PUBLIC $<TARGET_PROPERTY:history,Fortran_MODULE_DIRECTORY>)
