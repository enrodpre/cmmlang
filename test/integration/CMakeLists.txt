cmake_minimum_required(VERSION 3.20)

message(STATUS "CWD: ${CMAKE_CURRENT_SOURCE_DIR}")
project(CmmIntegrationTests)

find_package(Python3 REQUIRED COMPONENTS Interpreter)


create_symlink(e2e.py)
create_symlink(conftest.py)

set(PYTEST_BASE_OPTS
    #--tb=short
    -s
    --no-header
    -qq
    --disable-warnings
    -rN
)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/generated)



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

  list(APPEND PYTEST_CMD --root-dir ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR}/e2e.py -v)

  # Create the target
  # add_test(NAME ${TARGET_NAME}
  #       COMMAND ${CMAKE_COMMAND} -E env ${ARGS_ENV_VARS} ${PYTEST_CMD}
  #       WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  #   )
  # set_tests_properties(${TARGET_NAME} PROPERTIES
  #           LABELS "integration;python"
  #           TIMEOUT 300
  #       )
  #
  # # Make sure C++ targets are built before running integration tests
  # set_tests_properties(${TARGET_NAME} PROPERTIES
  #           DEPENDS "CmmLang"  # Replace with your actual target name
  # )
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
