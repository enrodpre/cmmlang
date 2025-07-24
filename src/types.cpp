#include "types.hpp"

namespace cmm {

[[nodiscard]] std::string rtti::format() const {
  return std::string(type.format_str);
  // std::make_format_args(c ? "const " : "")); v ? "vol " : ""));
}

rtti::operator const type_index&() const {
  return identifier;
}

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

} // namespace cmm
