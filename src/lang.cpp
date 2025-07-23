#include "lang.hpp"
#include <sys/types.h>

[[nodiscard]] std::string cmm::operator_t::caller_function() const {
  return std::format("operator{}", value());
}

[[nodiscard]] std::string cmm::operator_t::format() const {
  return std::format("operator{}", repr);
}

namespace cmm::types {
enum_t::enum_t()
    : underlying_t(builder::create<uint_t>()) {}

builder::build_step builder::init() {
  return {};
}

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
} // namespace cmm::types
