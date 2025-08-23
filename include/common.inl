#pragma once

#include "common.hpp"

#include <algorithm>
#include <cassert>
#include <concepts>
#include <cpptrace/utils.hpp>
#include <cstddef>
#include <format>
#include <initializer_list>
#include <iterator>
#include <libassert/assert-macros.hpp>
#include <magic_enum/magic_enum.hpp>
#include <optional>
#include <ranges>
#include <string>
#include <type_traits>
#include <typeinfo>
#include <unordered_map>
#include <utility>
#include <vector>

#include "os.hpp"
#include "traits.hpp"

namespace cmm {
enum class compilation_error_t : uint8_t;
namespace log {
enum class style_t : uint8_t;
} // namespace log

[[nodiscard]] constexpr const compilation_error_data::properties_map&
compilation_error_data::properties_array() {
  using enum compilation_error_t;
  using namespace os;
  static_assert(std::is_constant_evaluated());
  static constexpr properties_map MAP{
      {{{GENERIC, os::status::GENERIC_ERROR, "Generic error", false},
        {INVALID_CONTINUE,
         os::status::INVALID_CONTINUE,
         "continue statement not within loop or switch",
         true},
        {INVALID_BREAK,
         os::status::INVALID_BREAK,
         "break statement not within loop or switch",
         true},
        {UNDECLARED_SYMBOL, os::status::UNDECLARED_SYMBOL, "{} not declared", true},
        {ALREADY_DECLARED_SYMBOL, os::status::ALREADY_DECLARED_SYMBOL, "{} already declared", true},
        {UNDEFINED_FUNCTION, os::status::COMPILATION_ERROR, "Function {} is not defined", true},
        {LABEL_IN_GLOBAL, os::status::LABEL_IN_GLOBAL, "Label {} in global scope", true},
        {RETURN_IN_GLOBAL, os::status::RETURN_IN_GLOBAL, "Return in global scope", true},
        {BAD_FUNCTION_CALL, os::status::BAD_FUNCTION_CALL, "Label {} in global scope", true},
        {WRONG_FUNCTION_ARGUMENT,
         os::status::WRONG_FUNCTION_ARGUMENT,
         "Wrong argument. Declared {}.",
         true},
        {UNEXPECTED_TOKEN, os::status::UNEXPECTED_TOKEN, "Unexpected token {}", true},
        {INCOMPATIBLE_TOKEN, os::status::INCOMPATIBLE_TOKEN, "Incompatible token {}", true},
        {REQUIRED_TYPE, os::status::REQUIRED_TYPE, "Required type in specifiers", true},
        {TOO_MANY_TYPES, os::status::UNEXPECTED_TOKEN, "More than one type in specifiers", true},
        {MISSING_ENTRY_POINT, os::status::MISSING_ENTRY_POINT, "Main function not found", false}}}};
  return MAP;
}; // namespace cmm

namespace log {
template <typename T>
constexpr std::string apply(T&& t, style_t s) {
  std::string pre;
  switch (s) {
    case style_t::HEADER:
      pre = "\033[40;1;35m";
      break;
    case style_t::BOLD:
      pre = "\033[1;33m";
      break;
    case style_t::ERROR:
      pre = "\033[1;38;2;205;92;92m";
      break;
    case style_t::RED:
      pre = "\033[0;31m";
      break;
    case style_t::MAGENTA:
      pre = "\033[0;35m";
      break;
    case style_t::YELLOW:
      pre = "\033[0;33m";
      break;
    case style_t::GREEN:
      pre = "\033[0;32m";
      break;
    case style_t::WHITE:
      pre = "\033[0;37m";
      break;
    case style_t::WHITE_SMOKE:
    case style_t::DARK_RED:
    case style_t::NORMAL:
      return t;
  };

  return std::format("{}{}{}", pre, t, "\033[0m");
}
} // namespace log

template <std::ranges::range T>
formattable_range<T>::formattable_range(T* t)
    : m_range(t) {}

static_assert(!PairLike<std::vector<int>>);
static_assert(EntryLike<std::unordered_map<int, int>>);

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

constexpr formattable::operator std::string() const { return format(); }

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

template <ScopedEnum E>
constexpr enumeration<E>::enumeration()
    : m_value(magic_enum::enum_values<E>()[0]) {}

template <ScopedEnum E>
constexpr auto enumeration<E>::type_name() noexcept {
  return magic_enum::enum_type_name<E>();
}
template <ScopedEnum E>
[[nodiscard]] constexpr auto enumeration<E>::name() const noexcept {
  return std::string(magic_enum::enum_name<E>(m_value));
};
template <ScopedEnum E>
[[nodiscard]] constexpr auto enumeration<E>::value() const noexcept {
  return magic_enum::enum_integer(m_value);
};

template <ScopedEnum From>
template <typename To>
[[nodiscard]] constexpr bool enumeration<From>::is_castable() const {

  if constexpr (Enumerable<To>) {
    if (auto to =
            magic_enum::enum_cast<typename To::value_type>(name(), magic_enum::case_insensitive)) {
      return to.has_value();
    }
  } else {
    if (auto to = magic_enum::enum_cast<To>(name(), magic_enum::case_insensitive)) {
      return to.has_value();
    }
  }
  return false;
}

template <ScopedEnum From>
template <typename To>
constexpr To enumeration<From>::cast() const {
  auto to_enum = name();
  if constexpr (Enumerable<To>) {
    if (auto to =
            magic_enum::enum_cast<typename To::value_type>(to_enum, magic_enum::case_insensitive)) {
      return to.value();
    }

    REGISTER_ERROR("Shouldnt cast {} to {} if it is not castable", name(), To::type_name());
    assert(false);
  } else {
    if (auto to = magic_enum::enum_cast<To>(to_enum, magic_enum::case_insensitive)) {
      return to.value();
    }

    REGISTER_ERROR(
        "Shouldnt cast {} to {} if it is not castable", name(), magic_enum::enum_type_name<To>());
    assert(false);
  }
}

template <ScopedEnum E>
constexpr std::string enumeration<E>::format() const {
  return std::format("{}::{}", type_name(), name());
}

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
void stack<T>::emplace_back(Args&&... args)
  requires std::is_constructible_v<T, Args...>
{
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
T stack<T>::pop_return() {
  // Check if T is move constructible
  if constexpr (std::is_move_constructible_v<T>) {
    T value = std::move(m_data.back()); // Move the last element
    m_data.pop_back();                  // Remove it from the container
    return value;                       // Return the moved value
  } else if constexpr (std::is_copy_constructible_v<T>) {
    T value = m_data.back(); // Copy the last element
    m_data.pop_back();       // Remove it from the container
    return value;            // Return the copied value
  } else if constexpr (std::is_copy_assignable_v<T>) {
    T value;
    value = m_data.back();
    m_data.pop_back(); // Remove it from the container
    return value;      // Return the copied value
  } else if constexpr (std::is_move_assignable_v<T>) {
    T value;
    value = std::move(m_data.back());
    m_data.pop_back(); // Remove it from the container
    return value;      // Return the copied value
  }
  ASSERT(false, "T is not movable either copyable");
}
template <typename T>
T&& stack<T>::pop_move() {
  auto&& ret = std::move(m_data.back());
  m_data.pop_back();
  return std::move(ret);
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

template <typename T>
vector<T>::vector(std::initializer_list<vector<T>::value_type> init)
    : m_data(init) {}

template <typename T>
vector<T>::vector(const container_type& t)
    : m_data(t) {}

template <typename T>
vector<T>::vector(container_type&& t)
    : m_data(std::move(t)) {}

template <typename T>
inline T& vector<T>::at(size_t i) {
  return m_data.at(i);
}

template <typename T>
inline const T& vector<T>::at(size_t i) const {
  return m_data.at(i);
}

template <typename T>
const vector<T>::container_type& vector<T>::data() const {
  return m_data;
}

template <typename T>
vector<T>::iterator vector<T>::begin() {
  return m_data.begin();
}

template <typename T>
vector<T>::iterator vector<T>::end() {
  return m_data.end();
}
template <typename T>
vector<T>::const_iterator vector<T>::begin() const {
  return m_data.begin();
}

template <typename T>
vector<T>::const_iterator vector<T>::end() const {
  return m_data.end();
}

template <typename T>
vector<T>::const_iterator vector<T>::cbegin() const {
  return m_data.cbegin();
}

template <typename T>
vector<T>::const_iterator vector<T>::cend() const {
  return m_data.cend();
}

template <typename T>
vector<T>::reverse_iterator vector<T>::rbegin() {
  return m_data.rbegin();
}
template <typename T>
vector<T>::reverse_iterator vector<T>::rend() {
  return m_data.rend();
}
template <typename T>
vector<T>::const_reverse_iterator vector<T>::rbegin() const {
  return m_data.rbegin();
}
template <typename T>
vector<T>::const_reverse_iterator vector<T>::rend() const {
  return m_data.rend();
}

template <typename T>
[[nodiscard]] bool vector<T>::empty() const {
  return m_data.empty();
}

template <typename T>
[[nodiscard]] size_t vector<T>::size() const {
  return m_data.size();
}
template <typename T>
void vector<T>::push_back(const T& t) {
  m_data.push_back(t);
}
template <typename T>
void vector<T>::push_back(T&& t) {
  m_data.push_back(std::move(t));
}
template <typename T>
template <typename Fn>
vector<T>::pointer_type vector<T>::find(Fn fn) {
  return *(m_data | std::ranges::find_if(fn));
}

template <typename T>
template <typename Fn>
vector<T>::const_pointer_type vector<T>::find(Fn fn) const {
  return *(m_data | std::ranges::find_if(fn));
}

template <typename T>
template <typename Fn>
auto vector<T>::transform(Fn&& fn) const {
  return m_data | std::views::transform(fn) | std::ranges::to<std::vector>();
}
template <typename T>
[[nodiscard]] std::string vector<T>::join(char delim, size_t lvl) const {
  return data() | std::views::transform([lvl](const auto& elem) {
           if constexpr (requires { elem.operator->(); }) {
             return elem->repr(lvl + 1);
           } else if constexpr (std::is_pointer_v<std::remove_cvref_t<decltype(elem)>>) {
             return elem->repr(lvl + 1);
           } else {
             return elem.repr(lvl + 1);
           }
         }) |
         std::views::join_with(delim) | std::ranges::to<std::string>();
}

template <typename T>
template <std::ranges::forward_range Pattern>
  requires(std::ranges::view<Pattern>)
std::string vector<T>::join(Pattern&& p, size_t lvl) const {
  return data() | std::views::transform([lvl](const auto& elem) {
           if constexpr (requires { elem.operator->(); }) {
             return elem->repr(lvl + 1);
           } else if constexpr (std::is_pointer_v<std::remove_cvref_t<decltype(elem)>>) {
             return elem->repr(lvl + 1);
           } else {
             return elem.repr(lvl + 1);
           }
         }) |
         std::views::join_with(p) | std::ranges::to<std::string>();
}
template <typename T>
template <std::move_constructible Func, std::ranges::forward_range Pattern>
  requires(std::ranges::view<Pattern>)
std::string vector<T>::join(Func&& fn, Pattern&& p, size_t) const {
  return data() | std::views::transform(fn) | std::views::join_with(p) |
         std::ranges::to<std::string>();
}

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
