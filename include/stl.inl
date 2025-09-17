#pragma once

#include "stl.hpp"
#include <ranges>

namespace cmm {

template <typename T>
stack<T>::stack(const container_type& other)
    : m_data(other) {}
template <typename T>
stack<T>::stack(container_type&& other)
    : m_data(std::move(other)) {}
template <typename T>
template <is_range_assignable<T> Range>
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
void stack<T>::swap(stack& other) noexcept {
  std::swap(m_data, other.m_data);
}

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
} // namespace cmm
