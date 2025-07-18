#include "lang.hpp"
#include <sys/types.h>

#define TYPE_FORMAT_IMPL(TYPE, stdstr, ...) \
  std::string TYPE::format() const { \
    return std::format(stdstr, \
                       std::format("{}{}", \
                                   base_type::const_ ? "const " : "", \
                                   base_type::volatile_ ? "volatile " : ""), \
                       ##__VA_ARGS__); \
  }

[[nodiscard]] std::string cmm::operator_t::caller_function() const {
  return std::format("operator{}", value());
}

[[nodiscard]] std::string cmm::operator_t::format() const {
  return std::format("operator{}", repr);
}

namespace cmm::types {

bool base_type::operator==(const base_type& other) const {
  return typeid(*this) == typeid(other);
}

bool indirection_t::operator==(const indirection_t& other) const {
  return typeid(*this) == typeid(other) && typeid(type_) == typeid(other.type_);
}

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

TYPE_FORMAT_IMPL(bool_t, "{}bool")
TYPE_FORMAT_IMPL(char_t, "{}char")
TYPE_FORMAT_IMPL(sint_t, "{}int")
TYPE_FORMAT_IMPL(uint_t, "{}unsigned int")
TYPE_FORMAT_IMPL(float_t, "{}float")
TYPE_FORMAT_IMPL(lvalue_ref_t, "{}{}", *type_)
// TYPE_FORMAT_IMPL(lvalue_ref_t, "{}{}&", *type_)
TYPE_FORMAT_IMPL(rvalue_ref_t, "{}{}", *type_)
// TYPE_FORMAT_IMPL(rvalue_ref_t, "{}{}&&", *type_)
TYPE_FORMAT_IMPL(pointer_t, "{}{}*", *type_)
TYPE_FORMAT_IMPL(array_t, "{}array<{}>", length)
TYPE_FORMAT_IMPL(function_t, "{}function<{}()>", *return_t)

} // namespace cmm::types
