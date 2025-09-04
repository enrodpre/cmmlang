# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file LICENSE.rst or https://cmake.org/licensing for details.

cmake_minimum_required(VERSION ${CMAKE_VERSION}) # this file comes with cmake

# If CMAKE_DISABLE_SOURCE_CHANGES is set to true and the source directory is an
# existing directory in our source tree, calling file(MAKE_DIRECTORY) on it
# would cause a fatal error, even though it would be a no-op.
if(NOT EXISTS "/home/kike/dev/cmm/build/_deps/statictypeinfo-src")
  file(MAKE_DIRECTORY "/home/kike/dev/cmm/build/_deps/statictypeinfo-src")
endif()
file(MAKE_DIRECTORY
  "/home/kike/dev/cmm/build/_deps/statictypeinfo-build"
  "/home/kike/dev/cmm/build/_deps/statictypeinfo-subbuild/statictypeinfo-populate-prefix"
  "/home/kike/dev/cmm/build/_deps/statictypeinfo-subbuild/statictypeinfo-populate-prefix/tmp"
  "/home/kike/dev/cmm/build/_deps/statictypeinfo-subbuild/statictypeinfo-populate-prefix/src/statictypeinfo-populate-stamp"
  "/home/kike/dev/cmm/build/_deps/statictypeinfo-subbuild/statictypeinfo-populate-prefix/src"
  "/home/kike/dev/cmm/build/_deps/statictypeinfo-subbuild/statictypeinfo-populate-prefix/src/statictypeinfo-populate-stamp"
)

set(configSubDirs )
foreach(subDir IN LISTS configSubDirs)
    file(MAKE_DIRECTORY "/home/kike/dev/cmm/build/_deps/statictypeinfo-subbuild/statictypeinfo-populate-prefix/src/statictypeinfo-populate-stamp/${subDir}")
endforeach()
if(cfgdir)
  file(MAKE_DIRECTORY "/home/kike/dev/cmm/build/_deps/statictypeinfo-subbuild/statictypeinfo-populate-prefix/src/statictypeinfo-populate-stamp${cfgdir}") # cfgdir has leading slash
endif()
