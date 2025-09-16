#pragma once

#include "common.hpp"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <format>
#include <iterator>
#include <magic_enum/magic_enum.hpp>
#include <optional>
#include <ranges>
#include <string>
#include <string_view>
#include <type_traits>
#include <typeinfo>
#include <utility>

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
  return m_range | std::ranges::views::transform(element_merger()) | std::views::join_with(d) |
         std::ranges::to<std::string>();
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
  auto demangled  = demangle(typeid(T).name());
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

template <typename T>
stack<T>::stack(const container_type& other)
    : m_data(other) {}
template <typename T>
stack<T>::stack(container_type&& other)
    : m_data(std::move(other)) {}
template <typename T>
template <is_range_asignable<T> Range>
stack<T>::stack(Range&& r)
    : m_data(std::ranges::to<container_type>(std::forward<Range>(r))) {}

template <typename T>
bool stack<T>::operator==(const stack& other) const noexcept {
  return m_data == other.m_data;
}
template <typename T>
bool stack<T>::operator!=(const stack& other) const noexcept {
  return m_data != other.m_data;
}

template <typename T>
stack<T>::iterator stack<T>::begin() noexcept {
  return m_data.rbegin();
}
template <typename T>
stack<T>::iterator stack<T>::end() noexcept {
  return m_data.rend();
}
template <typename T>
[[nodiscard]] stack<T>::const_iterator stack<T>::begin() const noexcept {
  return m_data.rbegin();
}
template <typename T>
[[nodiscard]] stack<T>::const_iterator stack<T>::end() const noexcept {
  return m_data.rend();
}
template <typename T>
[[nodiscard]] stack<T>::const_iterator stack<T>::cbegin() const noexcept {
  return m_data.crbegin();
}
template <typename T>
[[nodiscard]] stack<T>::const_iterator stack<T>::cend() const noexcept {
  return m_data.crend();
}
template <typename T>
[[nodiscard]] stack<T>::const_reverse_iterator stack<T>::crbegin() const noexcept {
  return m_data.cbegin();
}
template <typename T>
[[nodiscard]] stack<T>::const_reverse_iterator stack<T>::crend() const noexcept {
  return m_data.cend();
}

template <typename T>
T& stack<T>::top() noexcept {
  return m_data.back();
}
template <typename T>
[[nodiscard]] const T& stack<T>::top() const noexcept {
  return m_data.back();
}

template <typename T>
template <typename Func>
bool stack<T>::contains(Func&& func) const {
  return find_position(func).has_value();
}
template <typename T>
template <typename Func>
const T& stack<T>::find(Func func) const {
  return *(m_data | std::ranges::find_if(func));
}

template <typename T>
template <typename Func>
std::optional<size_t> stack<T>::find_position(Func func) const {
  auto it = m_data | std::ranges::find_if(func);
  if (it != m_data.cend()) {
    return std::distance(m_data.cbegin(), it);
  }
  return {};
}

template <typename T>
template <typename Func>
stack<T>::const_iterator stack<T>::find_all(Func func) const {
  return m_data | std::ranges::views::filter(func);
}
template <typename T>
template <typename Func>
size_t stack<T>::count(Func func) const {
  return std::count_if(m_data, func);
}
template <typename T>
template <typename... Args>
  requires std::is_constructible_v<T, Args...>
void stack<T>::emplace(Args&&... args) {
  m_data.emplace_back(std::forward<Args>(args)...);
}
template <typename T>
void stack<T>::push(const T& t) {
  return m_data.push_back(t);
}
template <typename T>
void stack<T>::push(T&& t) noexcept {
  return m_data.push_back(std::move(t));
}
template <typename T>
auto stack<T>::pop_value() {
  T t = std::move(top());
  pop();
  return t;
}

template <typename T>
void stack<T>::pop() {
  m_data.pop_back();
}
template <typename T>
void stack<T>::clear() noexcept {
  m_data.clear();
}
template <typename T>
void stack<T>::swap(stack& other) noexcept {
  std::swap(m_data, other.m_data);
}
template <typename T>
[[nodiscard]] constexpr size_t stack<T>::size() const noexcept {
  return m_data.size();
}
template <typename T>
[[nodiscard]] constexpr size_t stack<T>::max_size() const noexcept {
  return m_data.max_size();
}
template <typename T>
[[nodiscard]] constexpr bool stack<T>::empty() const noexcept {
  return m_data.empty();
}

// template <typename T, typename Alloc>
// vector<T, Alloc>::vector(std::initializer_list<vector<T, Alloc>::value_type> init)
//     : m_data(init) {}
//
// template <typename T, typename Alloc>
// vector<T, Alloc>::vector(const container_type& t)
//     : m_data(t) {}
//
// template <typename T, typename Alloc>
// vector<T, Alloc>::vector(container_type&& t)
//     : m_data(std::move(t)) {}

// template <typename T, typename Alloc>
// inline T& vector<T, Alloc>::at(size_t i) {
//   return m_data.at(i);
// }
//
// template <typename T, typename Alloc>
// inline const T& vector<T, Alloc>::at(size_t i) const {
//   return m_data.at(i);
// }
//
// template <typename T, typename Alloc>
// const vector<T, Alloc>::container_type& vector<T, Alloc>::data() const {
//   return m_data;
// }
//
// template <typename T, typename Alloc>
// vector<T, Alloc>::iterator vector<T, Alloc>::begin() {
//   return m_data.begin();
// }
//
// template <typename T, typename Alloc>
// vector<T, Alloc>::iterator vector<T, Alloc>::end() {
//   return m_data.end();
// }
// template <typename T, typename Alloc>
// vector<T, Alloc>::const_iterator vector<T, Alloc>::begin() const {
//   return m_data.begin();
// }
//
// template <typename T, typename Alloc>
// vector<T, Alloc>::const_iterator vector<T, Alloc>::end() const {
//   return m_data.end();
// }
//
// template <typename T, typename Alloc>
// vector<T, Alloc>::const_iterator vector<T, Alloc>::cbegin() const {
//   return m_data.cbegin();
// }
//
// template <typename T, typename Alloc>
// vector<T, Alloc>::const_iterator vector<T, Alloc>::cend() const {
//   return m_data.cend();
// }
//
// template <typename T, typename Alloc>
// vector<T, Alloc>::reverse_iterator vector<T, Alloc>::rbegin() {
//   return m_data.rbegin();
// }
// template <typename T, typename Alloc>
// vector<T, Alloc>::reverse_iterator vector<T, Alloc>::rend() {
//   return m_data.rend();
// }
// template <typename T, typename Alloc>
// vector<T, Alloc>::const_reverse_iterator vector<T, Alloc>::rbegin() const {
//   return m_data.rbegin();
// }
// template <typename T, typename Alloc>
// vector<T, Alloc>::const_reverse_iterator vector<T, Alloc>::rend() const {
//   return m_data.rend();
// }

// template <typename T, typename Alloc>
// [[nodiscard]] bool vector<T, Alloc>::empty() const {
//   return m_data.empty();
// }
//
// template <typename T, typename Alloc>
// [[nodiscard]] size_t vector<T, Alloc>::size() const {
//   return m_data.size();
// }
// template <typename T, typename Alloc>
// void vector<T, Alloc>::push_back(const T& t) {
//   m_data.push_back(t);
// }
// template <typename T, typename Alloc>
// void vector<T, Alloc>::push_back(T&& t) {
//   m_data.push_back(std::move(t));
// }
// template <typename T, typename Alloc>
// template <typename Fn>
// vector<T, Alloc>::pointer_type vector<T, Alloc>::find(Fn fn) {
//   return *(m_data | std::ranges::find_if(fn));
// }
//
// template <typename T, typename Alloc>
// template <typename Fn>
// vector<T, Alloc>::const_pointer_type vector<T, Alloc>::find(Fn fn) const {
//   return *(m_data | std::ranges::find_if(fn));
// }
//
// template <typename T, typename Alloc>
// template <typename Fn>
// auto vector<T, Alloc>::transform(Fn&& fn) const {
//   return m_data | std::views::transform(fn) | std::ranges::to<std::vector>();
// }
// template <typename T, typename Alloc>
// [[nodiscard]] std::string vector<T, Alloc>::join(char delim, size_t lvl) const {
//   return data() | std::views::transform([lvl](const auto& elem) {
//            if constexpr (requires { elem.operator->(); }) {
//              return elem->repr(lvl + 1);
//            } else if constexpr (std::is_pointer_v<std::remove_cvref_t<decltype(elem)>>) {
//              return elem->repr(lvl + 1);
//            } else {
//              return elem.repr(lvl + 1);
//            }
//          }) |
//          std::views::join_with(delim) | std::ranges::to<std::string>();
// }
//
// template <typename T, typename Alloc>
// template <std::ranges::forward_range Pattern>
//   requires(std::ranges::view<Pattern>)
// std::string vector<T, Alloc>::join(Pattern&& p, size_t lvl) const {
//   return data() | std::views::transform([lvl](const auto& elem) {
//            if constexpr (requires { elem.operator->(); }) {
//              return elem->repr(lvl + 1);
//            } else if constexpr (std::is_pointer_v<std::remove_cvref_t<decltype(elem)>>) {
//              return elem->repr(lvl + 1);
//            } else {
//              return elem.repr(lvl + 1);
//            }
//          }) |
//          std::views::join_with(p) | std::ranges::to<std::string>();
// }
// template <typename T, typename Alloc>
// template <std::move_constructible Func, std::ranges::forward_range Pattern>
//   requires(std::ranges::view<Pattern>)
// std::string vector<T, Alloc>::join(Func&& fn, Pattern&& p, size_t) const {
//   return data() | std::views::transform(fn) | std::views::join_with(p) |
//          std::ranges::to<std::string>();
// }

template <typename K, typename V>
hashmap<K, V>::hashmap()
    : m_store() {}

template <typename K, typename V>
bool hashmap<K, V>::contains(const K& id) const {
  return m_store.contains(id);
}

template <typename K, typename V>
void hashmap<K, V>::insert(const K& k, V&& v) {
  m_store[k] = std::move(v);
}
template <typename K, typename V>
void hashmap<K, V>::insert(const K& k, const V& v) {
  m_store[k] = v;
}
template <typename K, typename V>
hashmap<K, V>::value_type& hashmap<K, V>::at(const K& id) {
  return m_store.at(id);
}
template <typename K, typename V>
[[nodiscard]] const hashmap<K, V>::value_type& hashmap<K, V>::at(const K& id) const {
  return m_store.at(id);
}
template <typename K, typename V>
hashmap<K, V>::value_type& hashmap<K, V>::operator[](const key_type& k) {
  return m_store[k];
}
template <typename K, typename V>
const hashmap<K, V>::value_type& hashmap<K, V>::operator[](const key_type& k) const {
  return m_store[k];
}
template <typename K, typename V>
size_t hashmap<K, V>::size() const noexcept {
  return m_store.size();
}

template <typename K, typename V>
void hashmap<K, V>::clear() {
  m_store.clear();
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
