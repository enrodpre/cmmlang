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

