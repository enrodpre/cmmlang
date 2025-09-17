#pragma once

#include "common.hpp"

#include <cassert>
#include <cpptrace/cpptrace.hpp>
#include <magic_enum/magic_enum.hpp>
#include <ranges>
#include <typeinfo>

#include "traits.hpp"

namespace cmm {
enum class associativity_t : uint8_t;
namespace assembly {
enum class flag_t : uint16_t;
} // namespace assembly
namespace log {
enum class style_t : uint8_t;
} // namespace log

[[nodiscard]] constexpr const compilation_error_data::properties_map&
compilation_error_data::properties_array() {
  using enum compilation_error_t;
  static_assert(std::is_constant_evaluated());
  static constexpr properties_map MAP{
      {{{GENERIC, "Generic error", false},
        {INVALID_CONTINUE, "continue statement not within loop or switch", true},
        {INVALID_BREAK, "break statement not within loop or switch", true},
        {UNDECLARED_SYMBOL, "{} not declared", true},
        {ALREADY_DECLARED_SYMBOL, "{} already declared", true},
        {UNDEFINED_FUNCTION, "Function {} is not defined", true},
        {ALREADY_DEFINED_FUNCTION, "Function {} is already defined", true},
        {LABEL_IN_GLOBAL, "Label {} in global scope", true},
        {RETURN_IN_GLOBAL, "Return in global scope", true},
        {BAD_FUNCTION_CALL, "Label {} in global scope", true},
        {WRONG_FUNCTION_ARGUMENT, "Wrong argument. Declared {}.", true},
        {UNEXPECTED_TOKEN, "Unexpected token {}", true},
        {INCOMPATIBLE_TOKEN, "Incompatible token {}", true},
        {REQUIRED_TYPE, "Required type in specifiers", true},
        {TOO_MANY_TYPES, "More than one type in specifiers", true},
        {MISSING_ENTRY_POINT, "Main function not found", false},
        {NOT_BINDEABLE, "{} is not bindeable from value category {}", true}}}};
  return MAP;
}; // namespace cmm

template <std::ranges::range T>
formattable_range<T>::formattable_range(T* t)
    : m_range(t) {}

template <std::ranges::range T>
template <class Delim>
[[nodiscard]] constexpr std::string formattable_range<T>::join(Delim&& d) const {
  return m_range | std::ranges::views::transform(element_merger()) |
         std::ranges::views::join_with(d) | std::ranges::to<std::string>();
}

template <std::ranges::range T>
auto formattable_range<T>::element_merger() const {
  if constexpr (ScalarLike<T>) {
    return [](const auto& el) { return el.format(); };
  } else if constexpr (PairLike<T>) {
    return [](const auto& pair) { return std::format("{}, {}", pair.first, pair.second); };
  } else {
    return [](const auto& pair) { return std::format("{}, {}", pair.key, pair.value); };
  }
}

// constexpr formattable::operator std::string() const { return format(); }

namespace {
template <typename T>
std::string classname_only() {
  auto demangled  = cpptrace::demangle(typeid(T).name());
  auto last_colon = demangled.rfind("::");
  if (last_colon != std::string::npos) {
    return demangled.substr(last_colon + 2);
  }
  return demangled;
}
} // namespace

template <scoped_enum E>
template <scoped_enum From>
constexpr enumeration<E>::enumeration(From f)
    : m_value(magic_enum::enum_cast<E>(magic_enum::enum_name(f))) {}

template <scoped_enum E>
constexpr enumeration<E>::enumeration()
    : m_value(magic_enum::enum_values<E>()[0]) {}

template <scoped_enum From>
template <typename To>
constexpr To enumeration<From>::cast() const {
  auto to_enum = name();
  if constexpr (std::is_scoped_enum_v<To>) {
    if (auto to = magic_enum::enum_cast<To>(to_enum, magic_enum::case_insensitive)) {
      return to.value();
    }
  } else {
    if (auto to = magic_enum::enum_cast<To>(to_enum, magic_enum::case_insensitive)) {
      return to.value();
    }
  }
  REGISTER_ERROR(
      "Shouldnt cast {} to {} if it is not castable", name(), magic_enum::enum_type_name<To>());
  assert(false);
}

template <scoped_enum E>
constexpr std::string enumeration<E>::string() const {
  return std::format("{}::{}", type_name(), name());
}

[[nodiscard]] constexpr const instruction_data::properties_map&
instruction_data::properties_array() {
  using enum instruction_t;
  using enum instruction_result_reg;
  static constexpr properties_map MAP{{{
      {nop, 0, false, {}},            // nop
      {jmp, 1, false, {}},            //
      {je, 1, false, {}},             //
      {jne, 1, false, {}},            //
      {jz, 1, false, {}},             //
      {jnz, 1, false, {}},            // jnz
      {jg, 1, false, {}},             //
      {jge, 1, false, {}},            // jge
      {jl, 1, false, {}},             //
      {jle, 2, false, {}},            // jle
      {mov, 2, true, {LEFT}},         //
      {lea, 2, false, {LEFT}},        // lea
      {push, 1, false, {}},           //
      {pop, 1, false, {}},            //
      {cmp, 2, true, {}},             //
      {test, 2, true, {}},            // test
      {and_, 2, true, {}},            //
      {or_, 2, true, {}},             // or
      {xor_, 2, true, {}},            //
      {not_, 1, true, {}},            // not
      {inc, 1, false, {LEFT}},        //
      {dec, 1, false, {LEFT}},        // dec
      {add, 2, true, {LEFT}},         //
      {sub, 2, true, {LEFT}},         // sub
      {mul, 1, true, {ACCUMULATOR}},  // mul
      {imul, 1, true, {ACCUMULATOR}}, // imul
      {div, 1, true, {ACCUMULATOR}},  // div
      {idiv, 1, true, {ACCUMULATOR}}, // idiv
      {syscall, 0, false, {}},        // syscall
      {ret, 0, false, {}},            // ret
      {call, 1, false, {}},           // call
      {global, 1, false, {}},         // global
      {address_of, 1, true, {}},      // address_of
      {deref, 1, true, {}},           // deref
  }}};
  return MAP;
}
[[nodiscard]] constexpr const operator_data::properties_map& operator_data::properties_array() {
  using enum operator_t;
  using enum comparison_t;
  static constexpr properties_map MAP{{{

      // Arithmetics
      {plus,
       "+",
       6, // +
       associativity_t::L2R,
       {}},
      {minus,
       "-",
       6, // -
       associativity_t::L2R,
       {}},
      {star,
       "*",
       5, // *
       associativity_t::L2R,
       {}},
      {fslash,
       "/",
       5, // /
       associativity_t::L2R,
       {}},

      // Inc / dec
      {pre_inc,
       "++",
       3, // preinc
       associativity_t::R2L,
       {}},
      {pre_dec,
       "--",
       3, // predec
       associativity_t::R2L,
       {}},
      {post_inc,
       "++",
       2, // postinc
       associativity_t::L2R,
       {}},
      {post_dec,
       "--",
       2, // postdec
       associativity_t::L2R,
       {}},

      // Comparators
      {eq,
       "==",
       10, // ==
       associativity_t::L2R,
       EQ},

      {neq,
       "!=",
       10, // !=
       associativity_t::L2R,
       NE},
      {le,
       "<",
       9, // <
       associativity_t::L2R,
       LE},
      {lt,
       "<=",
       9, // <=
       associativity_t::L2R,
       LT},
      {gt,
       ">",
       9, // >
       associativity_t::L2R,
       GT},
      {ge,
       ">=",
       9, // >=
       associativity_t::L2R,
       GE},

      // Logical
      {xor_,
       "^",
       12, // ^
       associativity_t::L2R,
       {}},
      {or_,
       "||",
       15, // ||
       associativity_t::L2R,
       {}},
      {and_,
       "&&",
       14, // &&
       associativity_t::L2R,
       {}},
      {not_,
       "!",
       3, // !
       associativity_t::R2L,
       {}},
      {ampersand,
       "&",
       3, // &
       associativity_t::R2L,
       {}},
      // Assignment
      {assign,
       "=",
       16, // =
       associativity_t::R2L,
       {}}}}};

  return MAP;
}
[[nodiscard]] constexpr const comparison_data::properties_map& comparison_data::properties_array() {
  using enum comparison_t;
  using enum assembly::flag_t;
  using enum instruction_t;
  using enum operator_sign;
  static constexpr properties_map MAP{{{{EQ, NE, ZERO, SIGNED},
                                        {NE, EQ, ~ZERO, SIGNED},
                                        {LE, GT, SIGN | OVERFLOW, SIGNED},
                                        {LT, GE, ~SIGN | OVERFLOW, SIGNED},
                                        {GE, LT, SIGN | OVERFLOW, SIGNED},
                                        {GT, LE, ~SIGN | OVERFLOW, SIGNED},
                                        {U_LT, U_GT, CARRY, UNSIGNED},
                                        {U_GT, U_LT, ~CARRY, UNSIGNED}}}};
  return MAP;
}

}; // namespace cmm
