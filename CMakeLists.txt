cmake_minimum_required(VERSION 3.30 FATAL_ERROR)

set(LIBNAME
    CmmLib
    CACHE INTERNAL "")
set(APPNAME
    CmmLang
    CACHE INTERNAL "")

project(
  ${LIBNAME}
  VERSION "1.0.0"
  LANGUAGES CXX)


if(PROJECT_SOURCE_DIR STREQUAL PROJECT_BINARY_DIR)
  message(
    FATAL_ERROR
      "In-source builds not allowed. Please make a new directory (called a build directory) and run CMake from there."
  )
endif()

set(WARNING_LEVEL SOME CACHE STRING "The level of the enabled warnings")
set_property(CACHE WARNING_LEVEL PROPERTY STRINGS NONE SOME ALL ERROR)
option(BUILD_TESTING "Build tests")
option(USE_ASAN "Compile address sanitizer into the binary")
option(ENABLE_IWYU "Enable include-what-you-use analysis")
option(IWYU_AUTOFIX "Automatically apply IWYU suggestions")
option(BUILD_INTEGRATION "Build integration tests")

set(LOG_LEVELS "TRACE" CACHE STRING "Log level")
set_property(CACHE LOG_LEVELS PROPERTY STRINGS "TRACE" "DEBUG" "INFO" "WARN" "ERR" "CRITICAL" "OFF")
if(NOT LOG_LEVEL IN_LIST LOG_LEVELS)
  message(FATAL_ERROR "LOG_LEVEL must be valid, got ${LOG_LEVEL}, valid values: ${LOG_LEVELS}")
endif()

add_compile_definitions("SPDLOG_ACTIVE_LEVEL=SPD_LEVEL_${LOG_LEVEL}")

message(STATUS "Log level: ${LOG_LEVEL}")
message(STATUS "Warning level: ${WARNING_LEVEL}")

include(CPM)

file(GLOB_RECURSE sources CONFIGURE_DEPENDS
     "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
find_package(magic_enum REQUIRED)

CPMAddPackage(
  NAME Revisited
  SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/deps/Revisited"
)

if (NOT TARGET spdlog)
  find_package(spdlog REQUIRED)
endif()

find_package(cpptrace REQUIRED)


# Apply warning flags depending on selection
if(WARNING_LEVEL STREQUAL "NONE")
  message(STATUS "Building with no warnings")
elseif(WARNING_LEVEL STREQUAL "SOME")
  message(STATUS "Building with -Wall")
  add_compile_options(-Wall)
elseif(WARNING_LEVEL STREQUAL "ALL")
  message(STATUS "Building with -Wall -Wextra -Wpedantic -Werror -Wshadow")
  add_compile_options(-Wall -Wextra -Wpedantic -Werror -Wshadow)
elseif(WARNING_LEVEL STREQUAL "ERROR")
  message(STATUS "Building with all warnings and treating them as errors")
  add_compile_options(-Wall -Wextra -Wpedantic -Werror -Wshadow -Werror)
else()
  message(FATAL_ERROR "Invalid value for WARNING_LEVEL: ${WARNING_LEVEL}")
endif()

add_compile_options(-Werror)
if (CMAKE_CXX_COMPILER STREQUAL "/usr/bin/clang++")
  add_compile_options(-ferror-limit=10)
endif()

add_compile_options(-fdiagnostics-color=always -O0 -g -fno-inline -Wno-switch -fdiagnostics-show-option)
add_library(${LIBNAME} STATIC ${sources})
include(cotire)
cotire(${LIBNAME})
function(create_symlink file)
  file(CREATE_LINK
    ${CMAKE_CURRENT_SOURCE_DIR}/${file}
    ${CMAKE_CURRENT_BINARY_DIR}/${file}
    SYMBOLIC
  )
endfunction()

create_symlink(examples)

# target_compile_definitions(CmmLib PUBLIC SPDLOG_ACTIVE_LEVEL=)
target_link_libraries(
   ${LIBNAME} PUBLIC magic_enum::magic_enum
                           Revisited spdlog::spdlog cpptrace::cpptrace)
target_include_directories(
  ${LIBNAME}
  PUBLIC $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}-${PROJECT_VERSION}>)
target_compile_options(${LIBNAME} PRIVATE -fpch-preprocess)
add_executable(${APPNAME} main.cpp)
find_package(cxxopts REQUIRED)
target_link_libraries(${APPNAME} PRIVATE ${LIBNAME} cxxopts::cxxopts)
# target_precompile_headers(${APPNAME} REUSE_FROM ${LIBNAME})

if(BUILD_TESTING)
  enable_testing()
  add_subdirectory(test)
endif()

if(BUILD_INTEGRATION)
  add_subdirectory(test/integration)
endif()

if(USE_ASAN)
  include(cmake/sanitizers.cmake)
  add_sanitizer_support(address)
  add_compile_options(-fsanitize=address -fsanitize=undefined)
endif()

if(USE_IWYU)
  set(CMAKE_CXX_INCLUDE_WHAT_YOU_USE
"include-what-you-use;-w;-Xiwyu;--verbose=7")
  add_iwyu_to_target(CmmLang)
endif()

add_custom_command(TARGET CmmLang POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E create_symlink
        ${CMAKE_CURRENT_BINARY_DIR}/CmmLang
        ${CMAKE_SOURCE_DIR}/CmmLang
    COMMENT "Creating symlink to built file"
)
