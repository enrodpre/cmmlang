#include "common.hpp"

#define TYPE_INDEX(cls) std::type_index(typeid(cls))
namespace cmm {
[[nodiscard]] std::string location::format() const {
  return std::format("({}, {})", rows, cols);
}

} // namespace cmm
