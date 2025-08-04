#include "lang.hpp"
#include "types.hpp"
#include <sys/types.h>

// std::size_t cmm::type_info::hash() const {
//   return std::hash<cmm::type_info>{}(*this);
// }
namespace cmm {
[[nodiscard]] std::string operator_t::caller_function() const {
  return std::format("operator{}", repr);
}

[[nodiscard]] std::string operator_t::format() const { return std::format("operator{}", repr); }

// constexpr type_t typeof ::operator()(type) { return type_t::nullptr_t; }

// constexpr category_t typeof ::operator()(const type& t) { return t.category; }
constexpr size_t sizeof_::operator()(const type& t) {
  using enum type_category_t;
  switch (t.category) {
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
    case void_t:
    case scoped_enum_t:
    case unscoped_enum_t:
    case class_t:
      return 0;
    case type_category_t::any_t:
    case type_category_t::fundamental_t:
    case type_category_t::arithmetic_t:
    case type_category_t::integral_t:
    case type_category_t::compound_t:
    case type_category_t::indirection_t:
    case type_category_t::reference_t:
    case type_category_t::enum_t:
    default:
      break;
  }
  return 0;
}
constexpr size_t sizeof_::operator()(const object& o) { return sizeof_::operator()(o.type); }
} // namespace cmm
