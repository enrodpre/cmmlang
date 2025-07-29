#include "types.hpp"
#include <utils.hpp>

namespace cmm {
type::type(bool c, bool v)
    : m_const(c),
      m_volatile(v) {}
void_type::void_type(bool c, bool v)
    : fundamental_type(c, v) {}
bool_type::bool_type(bool c, bool v)
    : cmm::integral_type(c, v) {}
char_type::char_type(bool c, bool v)
    : cmm::integral_type(c, v) {}
sint_type::sint_type(bool c, bool v)
    : cmm::integral_type(c, v) {}
uint_type::uint_type(bool c, bool v)
    : cmm::integral_type(c, v) {}
float_type::float_type(bool c, bool v)
    : cmm::arithmetic_type(c, v) {}
compound_type::compound_type(bool c, bool v, cv_type r)
    : type(c, v),
      m_underlying(r) {}
lvalue_ref_type::lvalue_ref_type(bool c, bool v, cv_type r)
    : reference_type(c, v, r) {}
rvalue_ref_type::rvalue_ref_type(bool c, bool v, cv_type r)
    : reference_type(c, v, r) {}
pointer_type::pointer_type(bool c, bool v, cv_type r)
    : compound_type(c, v, r) {}

[[nodiscard]] std::string type::name() const noexcept {
  return cpptrace::demangle(typeid(this).name());
};
[[nodiscard]] bool type::is_same_as(cv_type other) const noexcept {
  REGISTER_INFO("Comparing {} with {}", other->name(), name());
  return other->name() == name();
}

type_index::type_index(type_t t, size_t s)
    : type(std::move(t)),
      value(s) {}

builder::build_step builder::init() { return {}; }

builder::build_step& builder::build_step::const_qual() {
  steps.push_back(step::const_);
  return *this;
}

builder::build_step& builder::build_step::volatile_qual() {
  steps.push_back(step::volatile_);
  return *this;
}

builder::build_step& builder::build_step::pointer_to() {
  steps.push_back(step::pointer);
  return *this;
}

builder::build_step& builder::build_step::reference_of() {
  steps.push_back(step::reference);
  return *this;
}

} // namespace cmm
