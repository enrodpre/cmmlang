#pragma once

#include "types.hpp"

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

template <typename... Args>
std::string mangler::type(type_t t, bool c, bool v, Args... args) {
  switch (t.inner()) {
    case _type_t::bool_t:
      return boolean(c, v);
    case _type_t::char_t:
      return character(c, v);
    case _type_t::uint_t:
      return uint(c, v);
    case _type_t::sint_t:
      return sint(c, v);
    case _type_t::float_t:
      return floating(c, v);
    case _type_t::lvalue_ref_t:
      return lvalue(c, v, std::forward<Args>(args)...);
    case _type_t::rvalue_ref_t:
      return rvalue(c, v, std::forward<Args>(args)...);
    case _type_t::pointer_t:
      return pointer(c, v, std::forward<Args>(args)...);
    case _type_t::array_t:
    case _type_t::function_t:
    case _type_t::enum_t:
    case _type_t::scoped_enum_t:
    case _type_t::unscoped_enum_t:
    case _type_t::class_t:
    case _type_t::base_t:
    case _type_t::compound_t:
    case _type_t::indirection_t:
    case _type_t::reference_t:
    case _type_t::fundamental_t:
    case _type_t::void_t:
    case _type_t::nullptr_t:
    case _type_t::arithmetic_t:
    case _type_t::integral_t:
      break;
  }
}

constexpr bool is_const_v::operator()(cv_type t) { return t->is_const(); }

} // namespace cmm
