cmake_minimum_required(VERSION 3.30)

include(../cmake/cmm-config.cmake)

project(
  ${APPLICATION}
  VERSION 1.0.0
  LANGUAGES CXX)

include(../cmake/tools.cmake)
include(../cmake/CPM.cmake)
CPMAddPackage(
  GITHUB_REPOSITORY jarro2783/cxxopts
  VERSION 3.0.0
  OPTIONS "CXXOPTS_BUILD_EXAMPLES NO" "CXXOPTS_BUILD_TESTS NO" "CXXOPTS_ENABLE_INSTALL YES"
)
cpmaddpackage(NAME ${LIBRARY} SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/..)

find_package(cxxopts)

add_executable(${PROJECT_NAME} ${CMAKE_CURRENT_SOURCE_DIR}/main.cpp)
add_dependencies(${PROJECT_NAME} ${LIBRARY})
set_target_properties(${PROJECT_NAME} PROPERTIES OUTPUT_NAME "${PROJECT_NAME}")
target_link_libraries(${PROJECT_NAME} ${LIBRARY} cxxopts::cxxopts)
