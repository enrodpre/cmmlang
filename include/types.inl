#pragma once

#include "types.hpp"
#include <type_traits>

namespace cmm {

[[nodiscard]] constexpr const type_t::properties_map& type_t::properties_array() {
  using enum _type_t;
  static constexpr properties_map MAP{
      {{{
            fundamental_t,
            "",
            base_t,
            {void_t, nullptr_t, arithmetic_t},
        },
        {void_t, "void_t", fundamental_t, {}},
        {nullptr_t, "nullptr", fundamental_t, {}},
        {arithmetic_t, "", fundamental_t, {integral_t, float_t}},
        {integral_t, "", arithmetic_t, {bool_t, char_t, uint_t, sint_t}},
        {bool_t, "{}{}bool", integral_t, {}},
        {char_t, "{}{}char", integral_t, {}},
        {uint_t, "{}{}int", integral_t, {}},
        {
            sint_t,
            "{}{}unsigned int",
            integral_t,
            {},
        },
        {float_t, "{}{}float", arithmetic_t, {}},
        {compound_t, "{}{}{}&", base_t, {}},
        {indirection_t, "", compound_t, {reference_t, pointer_t}},
        {reference_t, "", indirection_t, {lvalue_ref_t, rvalue_ref_t}},
        {lvalue_ref_t, "{}{}{}&&", reference_t, {}},
        {rvalue_ref_t, "", reference_t, {}},
        {pointer_t, "{}{}{}*", indirection_t, {}},
        {
            array_t,
            "{}{}array<{}>",
            compound_t,
            {},
        },
        {function_t, "{}{}function<{}()>", compound_t, {}},
        {enum_t, "", compound_t, {scoped_enum_t, unscoped_enum_t}},
        {scoped_enum_t, "{}enum {}", enum_t, {}},
        {unscoped_enum_t, "{}enum class {}", enum_t, {}},
        {class_t, "{}class {}", compound_t, {}}}}};
  return MAP;
}

constexpr std::string type::format() const {
  switch (category) {
    case category_t::void_t:
      return "void";
    case category_t::nullptr_t:
      return "nullptr_t";
    case category_t::bool_t:
      return std::format("{}{}bool", c ? "c" : "", v ? "v" : "");
    case category_t::char_t:
      return std::format("{}{}char", c ? "c" : "", v ? "v" : "");
    case category_t::uint_t:
      return std::format("{}{}uint", c ? "c" : "", v ? "v" : "");
    case category_t::sint_t:
      return std::format("{}{}sint", c ? "c" : "", v ? "v" : "");
    case category_t::float_t:
      return std::format("{}{}float", c ? "c" : "", v ? "v" : "");
    case category_t::lvalue_ref_t:
      return std::format("{}&{}", *underlying, c ? "c" : "", v ? "v" : "");
    case category_t::rvalue_ref_t:
      return std::format("{}&&{}", *underlying, c ? "c" : "", v ? "v" : "");
    case category_t::pointer_t:
      return std::format("{}*{}", *underlying, c ? "c" : "", v ? "v" : "");
    case category_t::array_t:
      return std::format("<{}, {}>", *underlying, rank);
    case category_t::scoped_enum_t:
    case category_t::unscoped_enum_t:
      return std::format("<{}, {}>", underlying, rank);
    case category_t::class_t:
      return std::format("{}", *underlying);
    case category_t::function_t:
    default:
      NOT_IMPLEMENTED;
  }
}

constexpr bool is_const_v::operator()(const type& t) { return t.c; }
constexpr type type_factory::create_fundamental(category_t cat, bool b, bool v) {
  static_assert(std::is_constant_evaluated());
  return type{cat, b, v};
}
constexpr type type_factory::create_pointer(const type& t, bool b, bool v) {
  static_assert(std::is_constant_evaluated());
  return type{category_t::pointer_t, t, b, v};
}
constexpr type type_factory::create_lvalue(const type& t, bool b, bool v) {
  static_assert(std::is_constant_evaluated());
  return type{category_t::lvalue_ref_t, t, b, v};
}

} // namespace cmm
