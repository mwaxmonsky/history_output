cmake_minimum_required(VERSION 3.17)

project(History-Output VERSION 0.0.1 LANGUAGES Fortran)
option(HISTORY_ENABLE_TESTS "Run pFUnit unit tests" OFF)
option(HISTORY_ENABLE_CODE_COVERAGE "Run code coverage tool" OFF)
set(DCMAKE_BUILD_TYPE Debug)
add_subdirectory(src)

if(HISTORY_ENABLE_TESTS)
  enable_testing()
  add_subdirectory(test)
endif()

if(HISTORY_ENABLE_CODE_COVERAGE)
  target_compile_options(hisotry -O0 --coverage)
endif()