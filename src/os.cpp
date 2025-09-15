#include "os.hpp"

#include <stdlib.h>

namespace cmm::os {

int execute(const std::string& command) { return system(command.data()); }

} // namespace cmm::os
