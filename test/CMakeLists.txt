# ---- Project ----
cmake_minimum_required(VERSION 3.20)

set(TESTSNAME
    CmmTests
    CACHE INTERNAL "")

message(STATUS "CWD: ${CMAKE_CURRENT_SOURCE_DIR}")

project(${TESTSNAME} LANGUAGES CXX)

set(GTEST_DISABLE_PTHREADS ON)
add_definitions(-DGTEST_PRINT_ENABLED)
file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/generated")

# ---- Dependencies ----
# include(../cmake/CPM.cmake)

find_package(GTest REQUIRED QUIET)

# include(GoogleTest)

# ---- Unit test function ----
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DGTEST_PRINT_ENABLED")
# ---- Add unit tests ----
# add_unit_test(parser)
set(UNIT_TEST_LIST state strings token preprocessor ast asm
types parser)
foreach(file ${UNIT_TEST_LIST})
  set(name unit_${file})
  add_executable(${name} "${CMAKE_CURRENT_SOURCE_DIR}/test_${file}.cpp")
  target_link_libraries(${name} CmmLib gtest gtest_main)
  gtest_discover_tests(${name})
  # set_tests_properties(${name} PROPERTIES LABELS "unit")
endforeach()

add_custom_target(run_unit_tests
COMMAND ${CMAKE_CTEST_COMMAND}
--output-on-failure
-VV
-C $<CONFIGURATION>
WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
COMMENT "Running all unit tests" )

set(ENV{PYTEST_INPUT_DIR} "${CMAKE_CURRENT_SOURCE_DIR}/../examples")
set(ENV{PYTEST_OUTPUT_DIR} "${CMAKE_CURRENT_BINARY_DIR}/generated")

function(create_symlink file)
  file(CREATE_LINK
    ${CMAKE_CURRENT_SOURCE_DIR}/${file}
    ${CMAKE_CURRENT_BINARY_DIR}/${file}
    SYMBOLIC
  )
endfunction()

# create_symlink(conftest.py)
create_symlink(e2e.py)

file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/../examples
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/generated)

# set(PYTEST_ARGS
#   --quiet
#   --tb=no
#   --disable-warnings
#   --no-header
#   --no-summary
#   -rN
# )

add_test(NAME e2e
  COMMAND pytest e2e.py
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

# set_tests_properties(e2e PROPERTIES LABELS "integration")

# ---- code coverage ----
if(ENABLE_TEST_COVERAGE)
  target_compile_options(${LIBRARY} PUBLIC -O0 -g -fprofile-arcs
                                           -ftest-coverage)
  target_link_options(${LIBRARY} PUBLIC -fprofile-arcs -ftest-coverage)
endif()
