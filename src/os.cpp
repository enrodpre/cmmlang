#include "os.hpp"
#include "logging.hpp"
#include <stdlib.h>

namespace cmm::os {

int execute(const std::string& command) {
  REGISTER_TRACE("Executing {}", command);
  return system(command.data());
}

} // namespace cmm::os
