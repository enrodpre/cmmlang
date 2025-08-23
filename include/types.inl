#pragma once


#include <utility>

#include "types.hpp"

namespace cmm {
enum class type_category_t : uint8_t;
struct type;

[[nodiscard]] constexpr const type_category_data::properties_map&
type_category_data::properties_array() {
  static constexpr properties_map MAP{
      {{{any_t, any_t, {fundamental_t, compound_t}},
        {fundamental_t, any_t, {void_t, nullptr_t, arithmetic_t}},
        {void_t, fundamental_t, {}},
        {nullptr_t, fundamental_t, {}},
        {arithmetic_t, fundamental_t, {integral_t, float_t}},
        {integral_t, arithmetic_t, {bool_t, char_t, uint_t, sint_t}},
        {bool_t, integral_t, {}},
        {char_t, integral_t, {}},
        {uint_t, integral_t, {}},
        {sint_t, integral_t, {}},
        {float_t, arithmetic_t, {}},
        {compound_t, any_t, {}},
        {indirection_t, compound_t, {reference_t, pointer_t}},
        {reference_t, indirection_t, {lvalue_ref_t, rvalue_ref_t}},
        {lvalue_ref_t, reference_t, {}},
        {rvalue_ref_t, reference_t, {}},
        {pointer_t, indirection_t, {}},
        {array_t, compound_t, {}},
        {function_t, compound_t, {}},
        {enum_t, compound_t, {scoped_enum_t, unscoped_enum_t}},
        {scoped_enum_t, enum_t, {}},
        {unscoped_enum_t, enum_t, {}},
        {class_t, compound_t, {}},
        {generic_t, generic_t, {}},
        {dummy_t, dummy_t, {}}}

      }};
  return MAP;
}

template <typename... Args>
ptype type::create(type_category_t t, Args&&... args) {
  return std::make_shared<const type>(t, std::forward<Args>(args)...);
}
} // namespace cmm
