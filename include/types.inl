#pragma once

#include "types.hpp"

namespace cmm::types {

constexpr category_data category_t::operator->() const { return category_data(underlying_type()); }
[[nodiscard]] constexpr const category_data::properties& category_data::get_properties() {
  using enum group_t;
  using enum core_t;
  using enum layer_t;
  // static constexpr properties MAP{{{{any_t, any_t, {fundamental_t, compound_t}},
  //                                   {fundamental_t, any_t, {void_t, nullptr_t, arithmetic_t}},
  //                                   {arithmetic_t, fundamental_t, {integral_t, float_t}},
  //                                   {integral_t, arithmetic_t, {bool_t, char_t, uint_t, sint_t}},
  //                                   {compound_t, any_t, {}},
  //                                   {indirection_t, compound_t, {reference_t, pointer_t}},
  //                                   {reference_t, indirection_t, {lvalue_ref_t, rvalue_ref_t}},
  //                                   {enum_t, compound_t, {scoped_enum_t, unscoped_enum_t}},
  //                                   {void_t, fundamental_t, {}},
  //                                   {nullptr_t, fundamental_t, {}},
  //                                   {bool_t, integral_t, {}},
  //                                   {char_t, integral_t, {}},
  //                                   {uint_t, integral_t, {}},
  //                                   {sint_t, integral_t, {}},
  //                                   {float_t, arithmetic_t, {}},
  //                                   {scoped_enum_t, enum_t, {}},
  //                                   {unscoped_enum_t, enum_t, {}},
  //                                   {class_t, compound_t, {}},
  //                                   {lvalue_ref_t, reference_t, {}},
  //                                   {rvalue_ref_t, reference_t, {}},
  //                                   {pointer_t, indirection_t, {}},
  //                                   {array_t, compound_t, {}},
  //                                   {function_t, compound_t, {}}
  //
  // }}};
  static constexpr properties MAP{{{{any_t, any_t},
                                    {fundamental_t, any_t},
                                    {arithmetic_t, fundamental_t},
                                    {integral_t, arithmetic_t},
                                    {compound_t, any_t},
                                    {indirection_t, compound_t},
                                    {reference_t, indirection_t},
                                    {enum_t, compound_t},
                                    {void_t, fundamental_t},
                                    {nullptr_t, fundamental_t},
                                    {bool_t, integral_t},
                                    {char_t, integral_t},
                                    {uint_t, integral_t},
                                    {sint_t, integral_t},
                                    {float_t, arithmetic_t},
                                    {scoped_enum_t, enum_t},
                                    {unscoped_enum_t, enum_t},
                                    {class_t, compound_t},
                                    {lvalue_ref_t, reference_t},
                                    {rvalue_ref_t, reference_t},
                                    {pointer_t, indirection_t},
                                    {array_t, compound_t},
                                    {function_t, compound_t}

  }}};
  return MAP;
}

template <ScopedEnum Enum>
constexpr magic_enum::string_view enum_strip_t(Enum e) {

  auto cat = magic_enum::enum_name(e);
  return std::format("is_{}", cat.substr(cat.size() - 2));
}
template <auto E>
constexpr std::string enum_strip_t() {
  auto cat = magic_enum::enum_name<E>();
  return std::format("is_{}", cat.substr(cat.size() - 2));
}

template <auto Cat>
  requires(ValueCategory<Cat>)
constexpr matcher is_category() {
  auto cat = magic_enum::enum_name<Cat>();
  return {std::format("is_{}", cat.substr(cat.size() - 2)), [](type_id t) -> match_result {
            auto ok = belongs_to(t->categorize(), Cat);
            return {
                ok,
                0,
            };
          }}; // namespace cmm::types
}

template <auto Cat>
  requires(ValueCategory<Cat>)
constexpr match_result is_category(type_id t) {
  return is_category<Cat>(t);
}

template <layer_t L>
modifier make_wrapper() {
  return {enum_strip_t<L>(), [](info t) -> info {
            t.layers.emplace(L);
            return t;
          }};
}; // namespace cmm::types

template <qualification_t L>
modifier make_qualifier() {
  return {enum_strip_t<L>(), [](info t) -> info {
            if (t.layers.empty()) {
              t.cv_qualifiers |= L;
            } else {
              t.layers.top().cv_qualifiers |= L;
            }
            return t;
          }};
}

template <layer_t L>
modifier make_peeler() {
  return {std::format("remove {}", enum_strip_t<L>()), [](info t) -> info {
            if (t.layers.top().tag == L) {
              t.layers.pop();
            }
            return t;
          }};
}

} // namespace cmm::types
