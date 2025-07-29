#pragma once

#include "common.hpp"
#include <type_traits>
#include <utils.hpp>

namespace cmm {

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
        pre = "\e[0;31m";
        break;
      case style_t::MAGENTA:
        pre = "\e[0;35m";
        break;
      case style_t::YELLOW:
        pre = "\e[0;33m";
        break;
      case style_t::GREEN:
        pre = "\e[0;32m";
        break;
      case style_t::WHITE:
        pre = "\e[0;37m";
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

template <typename Derived, typename P, typename... Parents>
identifiable<Derived, P, Parents...>::identifiable()
    : m_id(m_constructions++) {}

template <typename Derived, typename P, typename... Parents>
identifiable<Derived, P, Parents...>::~identifiable() {
  m_destructions++;
}

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

template <typename Derived, typename P, typename... Parent>
[[nodiscard]] std::string identifiable<Derived, P, Parent...>::id() const {
  return std::format("{}{}", classname_only<Derived>(), id_n());
}

template <typename Derived, typename P, typename... Parent>
[[nodiscard]] std::string identifiable<Derived, P, Parent...>::repr() const {
  if constexpr (sizeof...(Parent) > 0) {
    return id() + (identifiable<Parent>::repr() + ...);
  } else {
    return id();
  }
}

template <typename Derived, typename P, typename... Parents>
[[nodiscard]] size_t identifiable<Derived, P, Parents...>::id_n() const {
  return m_id;
}

template <typename Derived, typename P, typename... Parents>
size_t identifiable<Derived, P, Parents...>::constructed() {
  return m_constructions;
}
template <typename Derived, typename P, typename... Parents>
size_t identifiable<Derived, P, Parents...>::destructed() {
  return m_constructions;
}
template <typename Derived, typename P, typename... Parents>
size_t identifiable<Derived, P, Parents...>::active() {
  return m_constructions - m_destructions;
}
constexpr location::location(size_t row, size_t row_len, size_t col, size_t col_length)
    : rows({row, static_cast<size_t>(row_len - 1)}),
      cols({col, static_cast<size_t>(col_length - 1)}) {}
constexpr location::location(range a, range b)
    : rows(std::move(a)),
      cols(std::move(b)) {}
constexpr location location::operator+(const location& right) const {
  range new_rows = rows + right.rows;
  range new_cols = cols + right.cols;
  return {new_rows, new_cols};
}
template <ScopedEnum E>
constexpr enumeration<E>::enumeration()
    : m_value(magic_enum::enum_values<E>()[0]) {}

template <ScopedEnum E>
constexpr auto enumeration<E>::type_name() noexcept {
  return magic_enum::enum_type_name<E>();
}
template <ScopedEnum E>
[[nodiscard]] constexpr auto enumeration<E>::name() const noexcept {
  return magic_enum::enum_name<E>(m_value);
};
template <ScopedEnum E>
[[nodiscard]] constexpr auto enumeration<E>::value() const noexcept {
  return magic_enum::enum_integer(m_value);
};

template <ScopedEnum From>
template <typename To>
To enumeration<From>::cast() const {
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
std::string enumeration<E>::format() const {
  return std::format("{}::{}", type_name(), name());
}
template <typename T, typename V, auto N>
constexpr enum_property<T, V, N>::enum_property(T::value_type v)
    : m_value(v) {
  static_assert(std::is_default_constructible<V>());
}

template <typename T, typename V, auto N>
constexpr const V& enum_property<T, V, N>::read() const {
  auto tuple = T::properties_array().at(m_value);
  return std::get<N>(tuple);
}
template <typename T, typename V, auto N>
constexpr enum_property<T, V, N>::operator const V&() const {
  return read();
}
template <typename T, typename V, auto N>
std::string enum_property<T, V, N>::format() const {
  return std::format("{}", read());
}

template <typename T>
bool stack<T>::operator==(const stack& other) const noexcept {
  return m_data == other.m_data;
}
template <typename T>
bool stack<T>::operator!=(const stack& other) const noexcept {
  return m_data != other.m_data;
}

template <typename T> stack<T>::iterator stack<T>::begin() noexcept { return m_data.rbegin(); }
template <typename T> stack<T>::iterator stack<T>::end() noexcept { return m_data.rend(); }
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

template <typename T> template <typename Func>
bool stack<T>::contains(Func&& func) const {
  return find_position(func).has_value();
}
template <typename T> template <typename Func>
const T& stack<T>::find(Func func) const {
  return *(m_data | std::ranges::find_if(func));
}

template <typename T> template <typename Func>
std::optional<size_t> stack<T>::find_position(Func func) const {
  auto it = m_data | std::ranges::find_if(func);
  if (it != m_data.cend()) {
    return std::distance(m_data.cbegin(), it);
  }
  return {};
}

template <typename T>
template <typename Func> stack<T>::const_iterator stack<T>::find_all(Func func) const {
  return m_data | std::ranges::views::filter(func);
}
template <typename T>
template <typename Func> size_t stack<T>::count(Func func) const {
  return std::count_if(m_data, func);
}
template <typename T>
template <typename... Args> void stack<T>::emplace_back(Args&&... args)
  requires std::is_constructible_v<T, Args...>
{
  m_data.emplace_back(std::forward<Args>(args)...);
}
template <typename T>
void stack<T>::push(const T& t) noexcept {
  return m_data.push_back(std::move(t));
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
void vector<T>::push_back(T&& t) {
  m_data.push_back(std::move(t));
}
template <typename T>
void vector<T>::push_back(const T& t) {
  m_data.push_back(t);
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
template <typename... Args>
  requires(std::is_constructible_v<T, Args...>)
void vector<T>::emplace_back(Args&&... args) {
  m_data.emplace_back(std::forward<Args>(args)...);
}
}; // namespace cmm
