#pragma once

#include "common.hpp"
#include <functional>
#include <ranges>
#include <utils.hpp>

namespace cmm {

template <std::ranges::range T>
formattable_range<T>::formattable_range(T* t)
    : m_range(t) {}

static_assert(!PairLike<std::vector<int>>);
static_assert(EntryLike<std::unordered_map<int, int>>);

template <std::ranges::range T>
template <class Delim>
[[nodiscard]] constexpr std::string formattable_range<T>::join(
    Delim&& d) const {
  return m_range | std::ranges::views::transform(element_merger()) |
         std::views::join_with(d) | std::ranges::to<std::string>();
}

template <std::ranges::range T>
auto formattable_range<T>::element_merger() const {
  if constexpr (ScalarLike<T>) {
    return std::identity();
  } else if constexpr (PairLike<T>) {
    return [](const auto& pair) {
      return std::format("{}, {}", pair.first, pair.second);
    };
  } else {
    return [](const auto& pair) {
      return std::format("{}, {}", pair.key, pair.value);
    };
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
constexpr location::location(size_t row,
                             size_t row_len,
                             size_t col,
                             size_t col_length)
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
}; // namespace cmm
