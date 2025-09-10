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
option(WARNING_LEVEL "The set warning level")
option(USE_ASAN "Compile address sanitizer into the binary")
option(ENABLE_IWYU "Enable include-what-you-use analysis")
option(IWYU_AUTOFIX "Automatically apply IWYU suggestions")
option(BUILD_INTEGRATION "Build integration tests")

option(LOG_LEVEL "Log level" ${LOG_LEVEL})
if(NOT LOG_LEVEL MATCHES "^[0-5]$")
  message(FATAL_ERROR "LOG_LEVEL must be between 0 and 5, got: ${LOG_LEVEL}")
endif()
add_compile_definitions(-DLOG_LEVEL=${LOG_LEVEL})

message(STATUS "Log level: ${LOG_LEVEL}")

include(cotire)
include(CPM)

file(GLOB_RECURSE sources CONFIGURE_DEPENDS
     "${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp")
find_package(magic_enum REQUIRED)

CPMAddPackage(
  NAME Revisited
  SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/deps/Revisited"
)

# target_precompile_headers()

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

add_compile_options(-fdiagnostics-color=always -O0 -g -fno-inline -Wno-switch -fdiagnostics-show-option -ferror-limit=10)
add_library(${LIBNAME} STATIC ${sources})

function(create_symlink file)
  file(CREATE_LINK
    ${CMAKE_CURRENT_SOURCE_DIR}/${file}
    ${CMAKE_CURRENT_BINARY_DIR}/${file}
    SYMBOLIC
  )
endfunction()

create_symlink(examples)

target_compile_definitions(CmmLib PUBLIC SAVE_PREPROCESSED=0 SAVE_ASSEMBLY=1)
target_link_libraries(
   ${LIBNAME} PUBLIC magic_enum::magic_enum
                           Revisited )
target_include_directories(
  ${LIBNAME}
  PUBLIC $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
         $<INSTALL_INTERFACE:include/${PROJECT_NAME}-${PROJECT_VERSION}>)

add_executable(${APPNAME} main.cpp)
find_package(cxxopts REQUIRED)
target_link_libraries(${APPNAME} PRIVATE ${LIBNAME} cxxopts::cxxopts)

cotire(CmmLang)
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
