#pragma once

#include "ast.hpp"

namespace cmm::ast {

// template <typename T>
// siblings<T>::siblings()
//     : formattable_range<container_type>(&m_data) {}
//
template <typename T>
siblings<T>::siblings(std::initializer_list<T> init)
    : m_data(init) {}
// formattable_range<container_type>(&m_data) {}

template <typename T>
inline T& siblings<T>::at(size_t i) {
  return m_data.at(i);
}

template <typename T>
inline const T& siblings<T>::at(size_t i) const {
  return m_data.at(i);
}

template <typename T>
const siblings<T>::container_type& siblings<T>::data() const {
  return m_data;
}

template <typename T>
siblings<T>::iterator siblings<T>::begin() {
  return m_data.begin();
}

template <typename T>
siblings<T>::iterator siblings<T>::end() {
  return m_data.end();
}
template <typename T>
siblings<T>::const_iterator siblings<T>::begin() const {
  return m_data.begin();
}

template <typename T>
siblings<T>::const_iterator siblings<T>::end() const {
  return m_data.end();
}

template <typename T>
siblings<T>::const_iterator siblings<T>::cbegin() const {
  return m_data.cbegin();
}

template <typename T>
siblings<T>::const_iterator siblings<T>::cend() const {
  return m_data.cend();
}

template <typename T>
siblings<T>::reverse_iterator siblings<T>::rbegin() {
  return m_data.rbegin();
}
template <typename T>
siblings<T>::reverse_iterator siblings<T>::rend() {
  return m_data.rend();
}
template <typename T>
siblings<T>::const_reverse_iterator siblings<T>::rbegin() const {
  return m_data.rbegin();
}
template <typename T>
siblings<T>::const_reverse_iterator siblings<T>::rend() const {
  return m_data.rend();
}

template <typename T>
[[nodiscard]] bool siblings<T>::empty() const {
  return m_data.empty();
}

template <typename T>
[[nodiscard]] size_t siblings<T>::size() const {
  return m_data.size();
}
template <typename T>
void siblings<T>::push_back(T t) {
  m_data.push_back(t);
}
template <typename T>
template <typename Fn>
T* siblings<T>::find(Fn fn) {
  return *(m_data | std::ranges::find_if(fn));
}

template <typename T>
template <typename Fn>
const T* siblings<T>::find(Fn fn) const {
  return *(m_data | std::ranges::find_if(fn));
}

} // namespace cmm::ast
