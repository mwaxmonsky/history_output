cmake_minimum_required(VERSION 3.17)

project(History-Output VERSION 0.0.1 LANGUAGES Fortran)
option(HISTORY_ENABLE_TESTS "Run pFUnit unit tests" OFF)
option(HISTORY_ENABLE_CODE_COVERAGE "Run code coverage tool" OFF)

if(HISTORY_ENABLE_CODE_COVERAGE)
  set(CMAKE_Fortran_FLAGS "-O0 --coverage")
endif()

set(DCMAKE_BUILD_TYPE Debug)
add_subdirectory(src)

if(HISTORY_ENABLE_TESTS OR HISTORY_ENABLE_CODE_COVERAGE)
  enable_testing()
  add_subdirectory(test)
endif()
