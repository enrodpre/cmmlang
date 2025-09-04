# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file LICENSE.rst or https://cmake.org/licensing for details.

cmake_minimum_required(VERSION ${CMAKE_VERSION}) # this file comes with cmake

# If CMAKE_DISABLE_SOURCE_CHANGES is set to true and the source directory is an
# existing directory in our source tree, calling file(MAKE_DIRECTORY) on it
# would cause a fatal error, even though it would be a no-op.
if(NOT EXISTS "/home/kike/dev/cmm/deps/Revisited")
  file(MAKE_DIRECTORY "/home/kike/dev/cmm/deps/Revisited")
endif()
file(MAKE_DIRECTORY
  "/home/kike/dev/cmm/build/_deps/revisited-build"
  "/home/kike/dev/cmm/build/_deps/revisited-subbuild/revisited-populate-prefix"
  "/home/kike/dev/cmm/build/_deps/revisited-subbuild/revisited-populate-prefix/tmp"
  "/home/kike/dev/cmm/build/_deps/revisited-subbuild/revisited-populate-prefix/src/revisited-populate-stamp"
  "/home/kike/dev/cmm/build/_deps/revisited-subbuild/revisited-populate-prefix/src"
  "/home/kike/dev/cmm/build/_deps/revisited-subbuild/revisited-populate-prefix/src/revisited-populate-stamp"
)

set(configSubDirs )
foreach(subDir IN LISTS configSubDirs)
    file(MAKE_DIRECTORY "/home/kike/dev/cmm/build/_deps/revisited-subbuild/revisited-populate-prefix/src/revisited-populate-stamp/${subDir}")
endforeach()
if(cfgdir)
  file(MAKE_DIRECTORY "/home/kike/dev/cmm/build/_deps/revisited-subbuild/revisited-populate-prefix/src/revisited-populate-stamp${cfgdir}") # cfgdir has leading slash
endif()
