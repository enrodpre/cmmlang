cmake_minimum_required(VERSION 3.30 FATAL_ERROR)

set(LIBNAME
    CmmLib
    CACHE INTERNAL "")
set(APPNAME
    CmmLang
    CACHE INTERNAL "")

project(
  ${LIBNAME}
  VERSION "1.0.0")

if(PROJECT_SOURCE_DIR STREQUAL PROJECT_BINARY_DIR)
  message(
    FATAL_ERROR
      "In-source builds not allowed. Please make a new directory (called a build directory) and run CMake from there."
  )
endif()

find_program(IWYU_PATH NAMES include-what-you-use iwyu)
set (ENABLE_IWYU ON)
set (IWYU_AUTOFIX ON)
option(ENABLE_IWYU "Enable include-what-you-use analysis" OFF)
option(IWYU_AUTOFIX "Automatically apply IWYU suggestions" OFF)

# IWYU command line options
set(IWYU_OPTS
    # Basic options
    --verbose=1
    --max_line_length=100

    # Mapping files (optional)
    # --mapping_file=${CMAKE_SOURCE_DIR}/iwyu.imp

    # Analysis options
    --no_default_mappings
    --check_also="*.h"
    --check_also="*.hpp"
    --check_also="*.hxx"
)
add_custom_target(iwyu
    COMMAND ${CMAKE_COMMAND} -E echo "Running include-what-you-use analysis..."
    COMMENT "Analyzing includes with IWYU"
)
function(add_iwyu_to_target target_name)
  if(NOT IWYU_PATH OR NOT ENABLE_IWYU)
    return()
  endif()

  # Set IWYU as the compiler launcher for this target
  set_target_properties(${target_name} PROPERTIES
        CXX_INCLUDE_WHAT_YOU_USE "${IWYU_PATH};${IWYU_OPTS}"
    )

  # Get target sources for custom commands
  get_target_property(TARGET_SOURCES ${target_name} SOURCES)

  if(IWYU_AUTOFIX)
    # Create custom command to apply IWYU fixes
    add_custom_target(iwyu_fix_${target_name}
            COMMAND ${CMAKE_COMMAND} -E echo "Applying IWYU fixes to ${target_name}..."
            COMMAND ${CMAKE_SOURCE_DIR}/scripts/apply_iwyu_fixes.sh ${target_name}
            WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
            COMMENT "Applying IWYU fixes to ${target_name}"
            VERBATIM
        )

    # Make it depend on the main iwyu target
    if(TARGET iwyu)
      add_dependencies(iwyu iwyu_fix_${target_name})
    endif()
  endif()
endfunction()
include(cmake/cotire.cmake)

set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_SHELL, "/usr/bin/zsh")
include(cmake/CPM.cmake)

file(GLOB_RECURSE sources CONFIGURE_DEPENDS
     "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
CPMAddPackage(
  NAME Revisited
  GIT_REPOSITORY https://github.com/TheLartians/Visitor.git
  VERSION 2.0
)
find_package(magic_enum REQUIRED)
find_package(cpptrace REQUIRED)
find_package(libassert REQUIRED)
find_package(Revisited REQUIRED)
# find_package(frozen REQUIRED)

add_library(${LIBNAME} ${sources})
target_compile_options(${LIBNAME} PRIVATE
  -std=c++26
  -fdiagnostics-color=always
   -O0 -g -fno-inline
)
target_compile_features(${LIBNAME} PUBLIC cxx_std_26)
target_link_libraries(
   ${LIBNAME} PRIVATE magic_enum::magic_enum cpptrace::cpptrace
                          libassert::assert Revisited)
target_include_directories(
  ${LIBNAME}
  PUBLIC $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}-${PROJECT_VERSION}>)


add_executable(${APPNAME} main.cpp)
find_package(cxxopts REQUIRED)
target_compile_options(${APPNAME} PRIVATE
  -std=c++26
  -fdiagnostics-color=always
   -O0 -g -fno-inline
)
target_link_libraries(${APPNAME} PRIVATE ${LIBNAME} cxxopts::cxxopts)

if(BUILD_TESTS)
  enable_testing()
  add_subdirectory(test)
endif()

set(CMAKE_CXX_INCLUDE_WHAT_YOU_USE
"include-what-you-use;-w;-Xiwyu;--verbose=7")

# include(CPack)
# string(TOLOWER ${PROJECT_NAME}/version.h VERSION_HEADER_LOCATION)
#
# set(CPACK_PACKAGE_NAME ${})
# set(CPACK_PACKAGE_VENDOR "enrodpre")
# set(CPACK_PACKAGE_VERSION "1.0.0")
add_iwyu_to_target(CmmLang)
