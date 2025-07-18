include(FindPackageHandleStandardArgs)

# Checks an environment variable; note that the first check
# does not require the usual CMake $-sign.
if(DEFINED ENV{args_DIR})
  set(args_DIR "$ENV{args_DIR}")
endif()

find_path(
  args_INCLUDE_DIR
  args.hxx
  HINTS
  ${args_DIR}
)

find_package_handle_standard_args(
  args DEFAULT_MSG
  args_INCLUDE_DIR
)

if(args_FOUND)
  set(args_INCLUDE_DIRS ${args_INCLUDE_DIR})

  mark_as_advanced(
    args_INCLUDE_DIR
    args_DIR
  )
else()
  set(
    args_DIR "" CACHE STRING
    "args.hxx not found in standard locations."
  )
endif()
