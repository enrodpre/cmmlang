cmake_minimum_required(VERSION 3.20)

message(STATUS "CWD: ${CMAKE_CURRENT_SOURCE_DIR}")
project(CmmTests LANGUAGES CXX)

find_package(GTest)
include(GoogleTest)
target_link_libraries(GTest::gtest INTERFACE gtest_main)

file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/generated")
file(GLOB_RECURSE TEST_SOURCES "test_*.cpp")

foreach(unit ${TEST_SOURCES})
  get_filename_component(NAME ${unit} NAME_WLE)
  add_executable(${NAME} ${unit})
  target_compile_definitions(${NAME} PRIVATE -DGTEST_PRINT_ENABLED=1)
  target_link_libraries(${NAME} PRIVATE CmmLib GTest::gtest pthread)
  target_compile_options(${NAME} PRIVATE
        -g3                     # Maximum debug info
        -O0                     # No optimization
        -fno-omit-frame-pointer # Better stack traces
    )
  target_link_options(${NAME} PRIVATE
    -fsanitize=address
    -fsanitize=undefined)

  gtest_discover_tests(${NAME} ) #DISCOVERY_TIMEOUT 10 PROPERTIES LABELS "unit_tests")

  set_target_properties(${NAME} PROPERTIES CXX_STANDARD 26 CXX_STANDARD_REQUIRED ON)
  target_include_directories(${NAME}
    PRIVATE
    ${CMAKE_SOURCE_DIR}/src          # Access to your main source
    ${CMAKE_CURRENT_SOURCE_DIR}      # Access to test headers
)
endforeach()

include(sanitizers)
add_sanitizer_support(address)

include_directories(${GTEST_INCLUDE_DIRS})
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)

if(BUILD_INTEGRATION_TESTS)
  set(ENV{PYTEST_INPUT_DIR} "${CMAKE_CURRENT_SOURCE_DIR}/../examples")
  set(ENV{PYTEST_OUTPUT_DIR} "${CMAKE_CURRENT_BINARY_DIR}/generated")

  function(create_symlink file)
    file(CREATE_LINK
    ${CMAKE_CURRENT_SOURCE_DIR}/${file}
    ${CMAKE_CURRENT_BINARY_DIR}/${file}
    SYMBOLIC
  )
  endfunction()

  create_symlink(conftest.py)
  create_symlink(e2e.py)

  # add_custom_target(${NAME} COMMAND ${UNIT_NAME} --gtest-filter="${NAME}"
  #   DEPENDS ${UNIT_NAME} WORKING_DIRECTORY CMAKE_CURRENT_BINARY_DIR
  #   COMMENT "Run ${unit} test suite}")
  file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/../examples
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
  file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/generated)


  add_test(NAME e2e
  COMMAND pytest e2e.py
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

  add_custom_target(test_e2e
    COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure --label-regex "e2e" -VV
    COMMENT "Running integration tests"
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)
endif()

add_custom_target(test_all
    COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure -VV
    COMMENT "Running all tests"
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)
# ---- code coverage ----
if(ENABLE_TEST_COVERAGE)
  target_compile_options(${LIBRARY} PUBLIC -O0 -g -fprofile-arcs
                                           -ftest-coverage)
  target_link_options(${LIBRARY} PUBLIC -fprofile-arcs -ftest-coverage)
endif()
