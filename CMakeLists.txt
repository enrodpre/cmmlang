cmake_minimum_required(VERSION 3.30 FATAL_ERROR)

project(
  ${LIBRARY}
  VERSION "1.0.0"
  LANGUAGES CXX)


if(PROJECT_SOURCE_DIR STREQUAL PROJECT_BINARY_DIR)
  message(
    FATAL_ERROR
      "In-source builds not allowed. Please make a new directory (called a build directory) and run CMake from there."
  )
endif()

include(cmake/CPM.cmake)

include(cmake/cotire.cmake)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake")

# include(cmake/cmm-config.cmake)

set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_SHELL, "/usr/bin/zsh")
set(CMAKE_CXX_FLAGS_DEBUG
    "${CMAKE_CXX_FLAGS_DEBUG} -O0 -g -fno-eliminate-unused-debug-symbols -fno-inline -fno-omit-frame-pointer"
)

file(GLOB_RECURSE sources CONFIGURE_DEPENDS
     "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")

find_package(magic_enum REQUIRED)
find_package(cpptrace REQUIRED)
find_package(libassert REQUIRED)

add_library(${PROJECT_NAME} ${sources})
set_target_properties(${PROJECT_NAME} PROPERTIES LINKER_LANGUAGE CXX)
add_compile_options(${PROJECT_NAME} PRIVATE
  -std=c++26
  -fdiagnostics-color=always
   -O0 -g3 -fno-inline
  ${COMPILE_ARGS}
)

target_compile_features(${PROJECT_NAME} PUBLIC cxx_std_26)

target_link_libraries(
  ${PROJECT_NAME} PUBLIC magic_enum::magic_enum cpptrace::cpptrace
                         libassert::assert $<$<BOOL:${MINGW}>:ws2_32>)
target_include_directories(
  ${PROJECT_NAME}
  PUBLIC $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}-${PROJECT_VERSION}>)


include(CPack)
string(TOLOWER ${PROJECT_NAME}/version.h VERSION_HEADER_LOCATION)

set(CPACK_PACKAGE_NAME ${PROJECT_NAME})
set(CPACK_PACKAGE_VENDOR "enrodpre")
set(CPACK_PACKAGE_VERSION "1.0.0")

