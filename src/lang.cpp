#include "lang.hpp"
#include "types.hpp"
#include <sys/types.h>

// std::size_t cmm::type_info::hash() const {
//   return std::hash<cmm::type_info>{}(*this);
// }
namespace cmm {
[[nodiscard]] std::string operator_t::caller_function() const {
  return std::format("operator{}", value());
}

[[nodiscard]] std::string operator_t::format() const { return std::format("operator{}", repr); }

constexpr type_t typeof ::operator()(cv_type) { return type_t::nullptr_t; }

constexpr size_t sizeof_::operator()(cv_type t) { return t->size(); }
constexpr size_t sizeof_::operator()(const type_t& t) {
  using enum _type_t;
  switch (t.inner()) {
    case lvalue_ref_t:
    case rvalue_ref_t:
    case nullptr_t:
    case pointer_t:
      return 8;
    case bool_t:
      return 1;
    case char_t:
      return 1;
    case uint_t:
    case sint_t:
      return 4;
    case float_t:
      return 8;
    case array_t:
    case function_t:
    case enum_t:
    case base_t:
    case compound_t:
    case indirection_t:
    case reference_t:
    case fundamental_t:
    case void_t:
    case arithmetic_t:
    case integral_t:
    case scoped_enum_t:
    case unscoped_enum_t:
    case class_t:
      return 0;
  }
}
constexpr size_t sizeof_::operator()(const object& o) {
  return sizeof_::operator()(typeof ::operator()(o.type));
}
} // namespace cmm
