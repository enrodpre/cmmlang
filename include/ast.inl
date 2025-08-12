#pragma once

#include "ast.hpp"

namespace cmm::ast {

// template <typename T>
// std ::string base_scope<T>::format() const {
//   return cpptrace::demangle(typeid(this).name());
// }
// template <typename T>
// std ::string base_scope<T>::repr(size_t n) const {
//   return std ::format(
//       "{}{}",
//       std ::string(n * 2, ' '),
//       std ::format("{}({}):\n{}", "Compound", vector<T>::size(), vector<T>::join('\n', n)));
// }

template <typename Scope>
[[nodiscard]] Scope&& scopes::translation_unit::active_scope() noexcept {
  if (m_stackframe.empty()) {
    return m_global_scope;
  }
  return active_frame().active_scope();
}
} // namespace cmm::ast
