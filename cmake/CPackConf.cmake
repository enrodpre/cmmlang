set(CPACK_SOURCE_GENERATOR TGZ)

set(CPACK_PACKAGE_NAME ${PROJECT_NAME})
set(CPACK_PACKAGE_VENDOR "Andrej Radović")
set(CPACK_PACKAGE_CONTACT "Andrej Radović")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "utility to get gcc (or other compiler) \
    diagnostics for a source file, with appropriate flags extracted from a \
    compilation database.")

set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE.txt")
set(CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
set(CPACK_OUTPUT_FILE_PREFIX "${CMAKE_CURRENT_BINARY_DIR}/package")
set(CPACK_PACKAGE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

# the project version is derived from the latest tag in the form of vX.Y.Z
set(CPACK_PACKAGE_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${PROJECT_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${PROJECT_VERSION_PATCH})

string(TOLOWER ${CMAKE_SYSTEM_NAME} _sys)
string(TOLOWER ${PROJECT_NAME} _project_lower)
set(CPACK_PACKAGE_FILE_NAME "${_project_lower}-${_sys}-")
set(CPACK_SOURCE_PACKAGE_FILE_NAME "${CPACK_PACKAGE_NAME}-${SEMVER}")

set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
set(CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/README.md")

# not a .gitignore format, escaped CMake regular expressions, one per line
file(READ ${CMAKE_CURRENT_LIST_DIR}/../.cpack_ignore _cpack_ignore)
string(REGEX REPLACE "\n" ";" _cpack_ignore ${_cpack_ignore})
set(CPACK_SOURCE_IGNORE_FILES "${_cpack_ignore}")

install(
  FILES ${CPACK_RESOURCE_FILE_README} ${CPACK_RESOURCE_FILE_LICENSE}
  DESTINATION share/docs/${PROJECT_NAME}
)

verbose_message("Generating SourceDist.cmake file.")
configure_file(
  "${CMAKE_CURRENT_LIST_DIR}/templates/SourceDist.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SourceDist.cmake"
  @ONLY
)

verbose_message("Generating SourceInstallScript.cmake file.")
configure_file(
  "${CMAKE_CURRENT_LIST_DIR}/scripts/SourceInstallScript.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/scripts/SourceInstallScript.cmake"
  @ONLY
)
set(
  CPACK_INSTALL_SCRIPT
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/scripts/SourceInstallScript.cmake"
)

include(CPack)
