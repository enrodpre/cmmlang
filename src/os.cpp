#include "os.hpp"
#include <spdlog/spdlog.h>
#include <stdlib.h>

namespace cmm::os {

int execute(const std::string& command) {
  SPDLOG_TRACE("Executing {}", command);
  return system(command.data());
}

} // namespace cmm::os
