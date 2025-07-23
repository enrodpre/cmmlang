#include "os.hpp"
#include <spdlog/spdlog.h>

namespace cmm::os {

void execute(const std::string& command) {
  int status = system(command.data());
  if (status != 0) {
    spdlog::error("Error while executing {}. Exiting", command);
    error(cmm::os::status::FAILED_EXTERN_COMMAND);
  }
}

} // namespace cmm::os
