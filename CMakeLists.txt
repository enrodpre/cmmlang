cmake_minimum_required(VERSION 3.30 FATAL_ERROR)

set(CMAKE_CXX_GENERATOR ninja)
set(CMAKE_BUILD_TYPE Debug)

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

set(WARNING_LEVEL SOME CACHE STRING "The level of the
enabled warnings")
set_property(CACHE WARNING_LEVEL PROPERTY STRINGS NONE SOME ALL ERROR)

option(BUILD_TESTS "Build tests" OFF)
option(BUILD_UNIT_TESTS "Build unit tests" OFF)
option(BUILD_INTEGRATION_TESTS "Build integration tests" OFF)
option(WARNING_LEVEL "The set warning level")
option(USE_ASAN "Compile address sanitizer into the binary")
option(ENABLE_IWYU "Enable include-what-you-use analysis")
option(IWYU_AUTOFIX "Automatically apply IWYU suggestions")


set(CXX_COMPILER ccache clang++)
set(CXX_COMPILER_ID Clang)
set(CXX_COMPILER_LAUNCHER ccache)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS ONN)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include(cotire)
include(CPM)

if(USE_ASAN)
  include(cmake/sanitizers.cmake)
  add_sanitizer_support(address)
  add_compile_options(-fsanitize=address)
endif()

file(GLOB_RECURSE sources CONFIGURE_DEPENDS
     "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
find_package(magic_enum REQUIRED)
find_package(Revisited REQUIRED)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/gdb")
add_subdirectory(gdb)

# Apply warning flags depending on selection
if(WARNING_LEVEL STREQUAL "NONE")
  message(STATUS "Building with no warnings")
elseif(WARNING_LEVEL STREQUAL "SOME")
  message(STATUS "Building with some warnings")
  add_compile_options(-Wall)
elseif(WARNING_LEVEL STREQUAL "ALL")
  message(STATUS "Building with all warnings")
  add_compile_options(-Wall -Wextra -Wpedantic -Werror -Wshadow)
elseif(WARNING_LEVEL STREQUAL "ERROR")
  message(STATUS "Building with all warnings and treating them as errors")
  add_compile_options(-Wall -Wextra -Wpedantic -Werror -Wshadow -Werror)
else()
  message(FATAL_ERROR "Invalid value for WARNING_LEVEL: ${WARNING_LEVEL}")
endif()

add_compile_options(-fdiagnostics-color=always -O0 -g -fno-inline -Wno-switch -fdiagnostics-show-option)
add_library(${LIBNAME} STATIC ${sources})

target_compile_definitions(CmmLib PUBLIC LOG_LEVEL=1 SAVE_PREPROCESSED=0 SAVE_ASSEMBLY=1)
target_link_libraries(
   ${LIBNAME} PUBLIC magic_enum::magic_enum
                           Revisited)
target_include_directories(
  ${LIBNAME}
  PUBLIC $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}-${PROJECT_VERSION}>)

add_executable(${APPNAME} main.cpp)
find_package(cxxopts REQUIRED)
target_link_libraries(${APPNAME} PRIVATE ${LIBNAME} cxxopts::cxxopts)

if(BUILD_TESTS)
  enable_testing()
  add_subdirectory(test)
endif()

# set(CMAKE_CXX_INCLUDE_WHAT_YOU_USE
# "include-what-you-use;-w;-Xiwyu;--verbose=7")
#
# add_iwyu_to_target(CmmLang)
