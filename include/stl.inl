#pragma once

#include "stl.hpp"
#include "strings.hpp"
#include <algorithm>
#include <ranges>
#include <type_traits>

namespace cmm {

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
    if (auto to = magic_enum::enum_cast<typename To::value_type>(
            to_enum, magic_enum::case_insensitive)) {
      return to.value();
    }

    spdlog::error("Shouldnt cast {} to {} if it is not castable",
                   name(),
                   To::type_name());
    assert(false);
  } else {
    if (auto to =
            magic_enum::enum_cast<To>(to_enum, magic_enum::case_insensitive)) {
      return to.value();
    }

    spdlog::error("Shouldnt cast {} to {} if it is not castable",
                   name(),
                   magic_enum::enum_type_name<To>());
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

template <typename T> stack<T>::iterator stack<T>::begin() noexcept {
  return m_data.rbegin();
}
template <typename T> stack<T>::iterator stack<T>::end() noexcept {
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
[[nodiscard]] stack<T>::const_reverse_iterator stack<T>::crbegin()
    const noexcept {
  return m_data.cbegin();
}
template <typename T>
[[nodiscard]] stack<T>::const_reverse_iterator stack<T>::crend()
    const noexcept {
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
template <typename Func> stack<T>::const_iterator stack<T>::find_all(
    Func func) const {
  return m_data | std::ranges::views::filter(func);
}
template <typename T>
template <typename Func> size_t stack<T>::count(Func func) const {
  return std::ranges::count_if(m_data, func);
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
}; // namespace cmm
