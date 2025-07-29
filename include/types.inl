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

constexpr bool is_const_v::operator()(cv_type t) { return t->is_const(); }

} // namespace cmm
