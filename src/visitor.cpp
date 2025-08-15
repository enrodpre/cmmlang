#include "visitor.hpp"

namespace cmm::ast {

template <is_node... Args>
composite::composite(Args&&... args) {
  (add(args), ...);
}

} // namespace cmm::ast
