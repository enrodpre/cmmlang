#pragma once

#include <cstdint>
#include <cstdlib>
#include <string>

namespace cmm::os {

enum class status : uint8_t {
  OK = 0,
  GENERIC_ERROR,
  LINKING_ERROR,
  NO_FILE_PROVIDED,
  FILE_NOT_FOUND,
  FILE_NOT_READABLE,
  FAILED_EXTERN_COMMAND,

  // Generic compilation errors
  MISSING_ENTRY_POINT,
  BAD_ENTRY_POINT,
  SYNTAX_ERROR,

  // Located compilation errors
  COMPILATION_ERROR,
  UNDECLARED_SYMBOL,
  UNDEFINED_SYMBOL,
  ALREADY_DECLARED_SYMBOL,
  ALREADY_DEFINED_SYMBOL,
  INVALID_BREAK,
  INVALID_CONTINUE,
  BAD_FUNCTION_CALL,
  WRONG_FUNCTION_ARGUMENT,
  UNEXPECTED_TOKEN,
  LABEL_IN_GLOBAL,
  RETURN_IN_GLOBAL,
  INCOMPATIBLE_TOKEN,
  REQUIRED_TYPE
};

[[noreturn]] inline void error(status status) {
  exit((uint8_t)(status));
}

void execute(const std::string&);

} // namespace cmm::os
