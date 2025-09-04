cmake_minimum_required(VERSION 3.20)

message(STATUS "CWD: ${CMAKE_CURRENT_SOURCE_DIR}")
project(CmmIntegrationTests)

find_package(Python3 REQUIRED COMPONENTS Interpreter)
file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/generated")

set(INTEGRATION_TEST_DIR ${CMAKE_SOURCE_DIR}/test/integration)
set(ENV{PYTEST_INPUT_DIR} "${CMAKE_CURRENT_SOURCE_DIR}/../examples")
set(ENV{PYTEST_OUTPUT_DIR} "${CMAKE_CURRENT_BINARY_DIR}/generated")

function(create_symlink file)
  file(CREATE_LINK
    ${CMAKE_CURRENT_SOURCE_DIR}/${file}
    ${CMAKE_CURRENT_BINARY_DIR}/${file}
    SYMBOLIC
  )
endfunction()

create_symlink(e2e.py)
create_symlink(conftest.py)

add_custom_target(run_pytest
    COMMAND ${Python3_EXECUTABLE} -m pytest
            -v                  # verbose output
            --tb=short          # shorter tracebacks
            --cov=src           # coverage for src directory
            --cov-report=html   # HTML coverage report
            tests/              # test directory
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    COMMENT "Running pytest with coverage and verbose output"
    VERBATIM
)

# Allow custom pytest arguments via CMake variables
set(PYTEST_COVERAGE "--cov=src --cov-report=html" CACHE STRING "Coverage arguments")
set(PYTEST_TARGET "tests/" CACHE STRING "Test target directory or file")

add_custom_target(run_pytest_custom
        COMMAND ${Python3_EXECUTABLE} -m pytest
                ${PYTEST_COVERAGE}
                ${PYTEST_TARGET}
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        COMMENT "Running pytest: ${PYTEST_ARGS} ${PYTEST_COVERAGE} ${PYTEST_TARGET}"
        VERBATIM
    )

set(PYTEST_BASE_OPTS
    -s
    #--tb=short
    --no-header
    -qq
    --disable-warnings
    --capture=no
)

file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/../examples
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/generated)


add_test(NAME single_example
  COMMAND pytest "${CMAKE_BINARY_DIR}/e2e.py"
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

# Custom function to create integration test targets
function(add_integration_test_target TARGET_NAME)
  set(options VERBOSE JSON_REPORT)
  set(oneValueArgs TEST_FILE WORKING_DIR)
  set(multiValueArgs PYTEST_ARGS ENV_VARS)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  # Set default working directory
  if(NOT ARGS_WORKING_DIR)
    set(ARGS_WORKING_DIR ${CMAKE_CURRENT_BINARY_DIR})
  endif()

  # Build pytest command
  set(PYTEST_CMD ${Python3_EXECUTABLE} -m pytest)

  # Add base options
  list(APPEND PYTEST_CMD ${PYTEST_BASE_OPTS})

  # Add JSON report if requested
  if(ARGS_JSON_REPORT)
    list(APPEND PYTEST_CMD
            --json-report
            --json-report-file=${ARGS_WORKING_DIR}/integration_report.json
        )
  endif()

  # Add custom pytest args
  if(ARGS_PYTEST_ARGS)
    list(APPEND PYTEST_CMD ${ARGS_PYTEST_ARGS})
  endif()

  list(APPEND PYTEST_CMD e2e.py)

  # Create the target
  add_custom_target(${TARGET_NAME}
        COMMAND ${CMAKE_COMMAND} -E echo "Setting up test environment..."
        COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/examples ${CMAKE_CURRENT_BINARY_DIR}/examples
        COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/conftest.py ${ARGS_WORKING_DIR}/ || ${CMAKE_COMMAND} -E true
        COMMAND ${CMAKE_COMMAND} -E echo "Running integration tests in ${ARGS_WORKING_DIR}..."
        COMMAND ${CMAKE_COMMAND} -E env ${ARGS_ENV_VARS} ${PYTEST_CMD}
        WORKING_DIRECTORY ${ARGS_WORKING_DIR}
        COMMENT "Running ${TARGET_NAME} in ${ARGS_WORKING_DIR}"
        USES_TERMINAL
    )
endfunction()
add_integration_test_target(integration)
add_custom_target(test-help
    COMMAND ${CMAKE_COMMAND} -E echo ""
    COMMAND ${CMAKE_COMMAND} -E echo "Available integration test targets:"
    COMMAND ${CMAKE_COMMAND} -E echo "  test-integration         - Run all integration tests"
    COMMAND ${CMAKE_COMMAND} -E echo "  test-integration-verbose - Run with verbose output"
    COMMAND ${CMAKE_COMMAND} -E echo "  test-integration-json    - Run with JSON report"
    COMMAND ${CMAKE_COMMAND} -E echo "  test-integration-env     - Run with custom environment"
    COMMAND ${CMAKE_COMMAND} -E echo "  test-integration-coverage- Run with coverage report"
    COMMAND ${CMAKE_COMMAND} -E echo "  test-integration-parallel- Run tests in parallel"
    COMMAND ${CMAKE_COMMAND} -E echo "  test-integration-api     - Run only API tests"
    COMMAND ${CMAKE_COMMAND} -E echo "  test-integration-database- Run only database tests"
    COMMAND ${CMAKE_COMMAND} -E echo "  clean-test              - Clean test build directory"
    COMMAND ${CMAKE_COMMAND} -E echo ""
    COMMAND ${CMAKE_COMMAND} -E echo "Working directory: ${TEST_BUILD_DIR}"
    COMMAND ${CMAKE_COMMAND} -E echo ""
    COMMENT "Show available test targets"
)
# ---- code coverage ----
if(ENABLE_TEST_COVERAGE)
  target_compile_options(${LIBRARY} PUBLIC -O0 -g -fprofile-arcs
                                           -ftest-coverage)
  target_link_options(${LIBRARY} PUBLIC -fprofile-arcs -ftest-coverage)
endif()
