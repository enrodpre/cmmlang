#include "visitor.hpp"

namespace cmm::ast {
leaf::leaf(cmm::location loc)
    : m_location(std::move(loc)) {}

leaf::leaf(const token& t)
    : m_location(t.location()) {}

template <is_node... Args>
composite::composite(Args&&... args) {
  (add(args), ...);
}

} // namespace cmm::ast
