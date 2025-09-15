#pragma once

#include <cstdint>
#include <string>

namespace cmm::os {

enum status_code : uint8_t {
  SUCCESS           = 0,
  GENERAL_ERROR     = 1,
  INVALID_ARGS      = 2,
  FILE_NOT_FOUND    = 3,
  PERMISSION_DENIED = 4,
  COMPILATION_ERROR = 5,
  ASSEMBLER_ERROR   = 6,
  LINKING_ERROR     = 7,
};

int execute(const std::string&);

} // namespace cmm::os
