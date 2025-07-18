#include "os.hpp"
#include "common.hpp"

namespace cmm::os {

void execute(const std::string& command) {
  int status = system(command.data());
  if (status != 0) {
    spdlog::error("Error while executing {}. Exiting", command);
    error(cmm::os::status::FAILED_EXTERN_COMMAND);
  }
}

} // namespace cmm::os
