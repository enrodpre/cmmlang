cmake_minimum_required(VERSION 3.30 FATAL_ERROR)

project(CmmGDB)

set(GDB_PROFILES compiler test asm)

# Function to generate GDB init files
function(generate_gdb_init_files)
  # Configure GDB init files
  foreach(PROFILE IN LISTS GDB_PROFILES)
    # Configure the gdbinit template
    configure_file(
            "${CMAKE_CURRENT_SOURCE_DIR}/${PROFILE}.gdbinit.in"
            "${CMAKE_BINARY_DIR}/gdb/${PROFILE}.gdbinit"
            @ONLY
        )
  endforeach()
endfunction()

# Function to create GDB launch targets
function(create_gdb_targets)
  # Create targets for each binary and profile combination
  foreach(TARGET ${ARGN})
    foreach(PROFILE IN LISTS GDB_PROFILES)
      # Create a unique target name
      set(GDB_TARGET_NAME "gdb-${TARGET}-${PROFILE}")

      # Add custom target for GDB
      add_custom_target(${GDB_TARGET_NAME}
                COMMAND ${GDB_EXECUTABLE}
                    -x "${CMAKE_BINARY_DIR}/gdb/${PROFILE}.gdbinit"
                    --args $<TARGET_FILE:${TARGET}>
                WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
                COMMENT "Running GDB for ${TARGET} with ${PROFILE} profile"
                VERBATIM
            )
    endforeach()
  endforeach()
endfunction()

# Generate init files during configuration
generate_gdb_init_files()

# Macro to expose GDB profile creation to other CMakeLists.txt
macro(add_gdb_profiles)
  create_gdb_targets(${ARGN})
endmacro()
