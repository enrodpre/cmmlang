#pragma once

#include "types.hpp"

#include <array>
#include <format>
#include <magic_enum/magic_enum.hpp>
#include <magic_enum/magic_enum_flags.hpp>

namespace cmm {
namespace types {
enum class core_t : enum_type;
enum class cv_qualification_t : uint8_t;
struct category_data;
struct match_result;
struct type_id;
} // namespace types
} // namespace cmm

namespace cmm::types {

template <>
constexpr const category_data* category_t::operator->() const {
  return &MANAGER.s_category_data.at(m_value);
}

// consteval std::array<types::type_id, 20> provide_builtins() { return {}; }

consteval decltype(manager::s_category_data) provide_categories() {
  using enum group_t;
  using enum core_t;
  using enum layer_t;
  constexpr decltype(manager::s_category_data) res{
      {MakeData<any_t, any_t>(),
       MakeData<fundamental_t, any_t, void_t, nullptr_t, arithmetic_t>(),
       MakeData<arithmetic_t, fundamental_t, integral_t, float_t>(),
       MakeData<integral_t, arithmetic_t, bool_t, char_t, uint_t, sint_t>(),
       MakeData<compound_t, any_t>(),
       MakeData<indirection_t, compound_t, reference_t, pointer_t>(),
       MakeData<reference_t, indirection_t, lvalue_ref_t, rvalue_ref_t>(),
       MakeData<void_t, fundamental_t>(),
       MakeData<enum_t, compound_t, scoped_enum_t, unscoped_enum_t>(),
       MakeData<nullptr_t, fundamental_t>(),
       MakeData<bool_t, integral_t>(),
       MakeData<char_t, integral_t>(),
       MakeData<uint_t, integral_t>(),
       MakeData<sint_t, integral_t>(),
       MakeData<float_t, arithmetic_t>(),
       MakeData<scoped_enum_t, enum_t>(),
       MakeData<unscoped_enum_t, enum_t>(),
       MakeData<class_t, compound_t>(),
       MakeData<lvalue_ref_t, reference_t>(),
       MakeData<rvalue_ref_t, reference_t>(),
       MakeData<pointer_t, indirection_t>(),
       MakeData<array_t, compound_t>(),
       MakeData<function_t, compound_t>()}
  };
  return res;
}

constexpr decltype(manager::s_category_data) manager::s_category_data = provide_categories();

template <auto Cat>
  requires CategoryValue<Cat>
constexpr unary_matcher is_category() {
  auto cat = magic_enum::enum_name<Cat>();
  return {std::format("is_{}", cat.substr(cat.size() - 2)),
          [](types::type_id t) -> match_result { return {belongs_to(t->categorize(), Cat)}; }};
}

template <auto Cat>
  requires(CategoryValue<Cat>)
constexpr match_result is_category(types::type_id t) {
  return is_category<Cat>()(t);
}

template <cv_qualification_t Cv>
constexpr unary_matcher is_cv_qualified() {
  return {std::format("is_", magic_enum::enum_name<cv_qualification_t>(Cv)),
          [](type_id t) -> match_result { return {magic_enum::enum_flags_test(t->cvqual(), Cv)}; }};
}
} // namespace cmm::types
