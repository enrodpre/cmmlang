cmake_minimum_required(VERSION 3.20)

message(STATUS "CWD: ${CMAKE_CURRENT_SOURCE_DIR}")
project(CmmTests LANGUAGES CXX)

find_package(GTest REQUIRED)
include(GoogleTest)
target_link_libraries(GTest::gtest INTERFACE gtest_main)

file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/generated")
file(GLOB_RECURSE TEST_SOURCES "test_*.cpp")

foreach(TESTFILE ${TEST_SOURCES})
  get_filename_component(NAME ${TESTFILE} NAME_WLE)
  add_executable(${NAME} ${TESTFILE})
  target_precompile_headers(${NAME} REUSE_FROM ${LIBNAME})
  target_compile_definitions(${NAME} PRIVATE -DGTEST_PRINT_ENABLED=1)
  target_link_libraries(${NAME} PRIVATE CmmLib GTest::gtest pthread)
  target_compile_options(${NAME} PRIVATE
        -g3                     # Maximum debug info
        -O0                     # No optimization
        -fno-omit-frame-pointer # Better stack traces
    )

  gtest_discover_tests(${NAME}  DISCOVERY_TIMEOUT 10 PROPERTIES LABELS "unit" )

  set_target_properties(${NAME} PROPERTIES CXX_STANDARD 26 CXX_STANDARD_REQUIRED ON)
  target_include_directories(${NAME}
    PRIVATE
    ${CMAKE_SOURCE_DIR}/src          # Access to your main source
    ${CMAKE_CURRENT_SOURCE_DIR}      # Access to test headers
)
endforeach()

include_directories(${GTEST_INCLUDE_DIRS})
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)

# ---- code coverage ----
if(ENABLE_TEST_COVERAGE)
  target_compile_options(CmmLang PRIVATE -O0 -g -fprofile-arcs
                                           -ftest-coverage)
  target_link_options(CmmLang PRIVATE -fprofile-arcs -ftest-coverage)
endif()
