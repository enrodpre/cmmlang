cmake_minimum_required(VERSION 3.30)

project(BuildAll LANGUAGES CXX)

# needed to generate test target
enable_testing()

add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/../standalone
                 ${CMAKE_BINARY_DIR}/standalone)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/../test ${CMAKE_BINARY_DIR}/test)
