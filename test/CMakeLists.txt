# ---- Project ----
cmake_minimum_required(VERSION 3.20)

set(TESTSNAME
    CmmTests
    CACHE INTERNAL "")

message(STATUS "CWD: ${CMAKE_CURRENT_SOURCE_DIR}")

project(${TESTSNAME} LANGUAGES CXX)

file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/generated")

# ---- Dependencies ----
# include(../cmake/CPM.cmake)

find_package(GTest REQUIRED QUIET)

# include(GoogleTest)

# ---- Unit test function ----
function(add_unit_test file)
  set(name unit_${file})
  add_executable(${name} "${CMAKE_CURRENT_SOURCE_DIR}/test_${file}.cpp")
  target_link_libraries(${name} CmmLib gtest gtest_main)
  gtest_discover_tests(${name})
endfunction()

# ---- Add unit tests ----
# add_unit_test(parser)
add_unit_test(state)
add_unit_test(strings)
add_unit_test(token)
add_unit_test(preprocessor)
add_unit_test(ast)
add_unit_test(asm)

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

# ---- code coverage ----
if(ENABLE_TEST_COVERAGE)
  target_compile_options(${LIBRARY} PUBLIC -O0 -g -fprofile-arcs
                                           -ftest-coverage)
  target_link_options(${LIBRARY} PUBLIC -fprofile-arcs -ftest-coverage)
endif()
