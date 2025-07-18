if(__CONFIG_INCLUDED)
  return()
endif()

set(__CONFIG_INCLUDED TRUE)


# if(!DEFINED ${USE_CCACHE})
#   set(USE_CCACHE ON)
# endif()

# option(${PROJECT_NAME}_ENABLE_CCACHE
#        "Enable the usage of Ccache, in order to speed up rebuild times." ON)
# find_program(CCACHE_FOUND ccache)
# if(CCACHE_FOUND)
#   set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ccache)
#   set_property(GLOBAL PROPERTY RULE_LAUNCH_LINK ccache)
# endif()

set(CMAKE_BUILD_TYPE "Debug")
if($<CXX_COMPILER_ID:GNU>)
  set(CMAKE_CXX_FLAGS_DEBUG -g3)
  set(CMAKE_C_FLAGS_DEBUG -g3)
endif()

option(${PROJECT_NAME}_ENABLE_CODE_COVERAGE "Enable code coverage through GCC."
       OFF)

option(
  ${PROJECT_NAME}_VERBOSE_OUTPUT
  "Enable verbose output, allowing for a better understanding of each step taken."
  ON)
option(${PROJECT_NAME}_GENERATE_EXPORT_HEADER
       "Create a `project_export.h` file containing all exported symbols." OFF)

set(CMAKE_COLOR_MAKEFILE ON)

set(LIBRARY
    CmmLib
    CACHE INTERNAL "")
set(APPLICATION
    CmmLang
    CACHE INTERNAL "")
set(TESTS
    CmmTests
    CACHE INTERNAL "")

option(ENABLE_ALL_WARNINGS "Compile with all warnings for the major compile
rs" OFF)
option(ENABLE_EFFECTIVE_CXX "Enable Effective C++ warnings" OFF)
option(GENERATE_DEPENDENCY_DATA "Generates .d files with header dependencie
s" OFF)

if(ENABLE_ALL_WARNINGS)
  add_compile_options(-Wall -Wextra)
endif()

if(ENABLE_EFFECTIVE_CXX)
  # set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Weffc++")
endif()

if(GENERATE_DEPENDENCY_DATA)
  add_compile_options(-MD)
endif()

# ucm_add_flags(-Wshadow -Woverloaded-virtual -Wuseless-cast -Wnull-dereference)

# message(STATUS "Building target ${PROJECT_NAME}")
# ucm_print_flags()
