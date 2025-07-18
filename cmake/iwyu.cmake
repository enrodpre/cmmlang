# INCLUDE-WHAT-YOU-USE
find_program(IWYU_EXE NAMES "include-what-you-use")
set(IWYU_MESSAGE_OUTPUT # Control output messages to occur only once
    FALSE
    CACHE INTERNAL FALSE)
mark_as_advanced(FORCE IWYU_EXE CMAKE_C_INCLUDE_WHAT_YOU_USE
                 CMAKE_CXX_INCLUDE_WHAT_YOU_USE)

# Adds include-what-you-use to code compiled after this macro. All arguments are
# added to the include-what-you-use application call in the form of
# `include-what-you-use ${ARGN}`.
#
# If the include-what-you-use application is not found, the macro will cause
# CMake to produce an error and not generate.
#
# Options provided can be changed by calling the macro again with the new
# arguments.
macro(include_what_you_use)
  # Only want to output whether clang-tidy was found once
  if(NOT IWYU_MESSAGE_OUTPUT)
    set(IWYU_MESSAGE_OUTPUT TRUE)
    if(IWYU_EXE)
      message(STATUS "include-what-you-use found: ${IWYU_EXE}")
    else()
      message(SEND_ERROR "include-what-you-use not found!")
    endif()
  endif()

  # Only pass the options if the tool was found
  if(IWYU_EXE)
    set(CMAKE_C_INCLUDE_WHAT_YOU_USE
        ${IWYU_EXE} ${ARGN}
        CACHE STRING "" FORCE)
    set(CMAKE_CXX_INCLUDE_WHAT_YOU_USE
        ${IWYU_EXE} ${ARGN}
        CACHE STRING "" FORCE)
  endif()
endmacro()

# Clears include-what-you-use so it is not called on any following defined code
# compilation. It can be re-enabled by another call to `include_what_you_use()`.
macro(reset_include_what_you_use)
  set(CMAKE_C_INCLUDE_WHAT_YOU_USE
      ""
      CACHE STRING "" FORCE)
  set(CMAKE_CXX_INCLUDE_WHAT_YOU_USE
      ""
      CACHE STRING "" FORCE)
endmacro()
