#pragma once

#include "common.hpp"
#include "traits.hpp"
#include <algorithm>
#include <functional>
#include <libassert/assert.hpp>
#include <magic_enum/magic_enum.hpp>
#include <magic_enum/magic_enum_containers.hpp>
#include <magic_enum/magic_enum_format.hpp>
#include <type_traits>
#include <utils.hpp>
#include <vector>

namespace cmm {

template <typename T>
std::string type_name() {
  return cpptrace::demangle(typeid(T).name());
}

using cstring = std::string_view;

template <typename T>
concept uint_comparable_t = std::convertible_to<T, uint8_t>;

// Equality operator
template <uint_comparable_t T>
bool operator==(const T& one, const T& other) {
  return (uint64_t)one == (uint64_t)other;
}

// Less than operator
template <uint_comparable_t T>
bool operator<(const T& one, const T& other) {
  return (uint64_t)one < (uint64_t)other;
}

// Greater than operator
template <uint_comparable_t T>
bool operator>(const T& one, const T& other) {
  return (uint64_t)one > (uint64_t)other;
}

// Less than or equal to operator
template <uint_comparable_t T>
bool operator<=(const T& one, const T& other) {
  return !((uint64_t)one > (uint64_t)other);
}

// Greater than or equal to operator
template <uint_comparable_t T>
bool operator>=(const T& one, const T& other) {
  return !((uint64_t)one > (uint64_t)other);
}

// Not equal operator
template <uint_comparable_t T>
bool operator!=(const T& one, const T& other) {
  return !((uint64_t)one == (uint64_t)other);
}

template <typename T, size_t N>
class generator {
  size_t counter{};
  std::array<T, N> data;
  std::function<T(T)> before_each_fn;

public:
  generator(
      decltype(data) d,
      decltype(before_each_fn) fn = [](T t) { return t; })
      : data(d),
        before_each_fn(fn) {}
  T next() { return before_each_fn(data[counter++]); }
  void reset() { counter = 0; }
};

template <typename T, typename Fn = void> class property {
public:
  explicit property(const T& v)
      : value(v) {}
  explicit property(T&& v)
      : value(std::move(v)) {}

  property& operator=(const T& v) { value = v; }
  property& operator=(T&& v) { value = std::move(v); }
  property& operator=(const property& other) { this->value = other.value; }
  operator const T&() const { return value; }
  operator T() const { return value; }

private:
  property(const property&) = default;
  T value;
};

template <typename T>
class member_ptr {

public:
  member_ptr() = default;
  template <typename U>
    requires std::is_convertible_v<U*, T*>
  member_ptr(std::unique_ptr<U>&&);
  template <typename U>
    requires std::is_convertible_v<U*, T*>
  explicit member_ptr(U*);
  ~member_ptr();
  member_ptr(member_ptr&&)            = default;
  member_ptr& operator=(member_ptr&&) = default;
  T* get() const;
  T* operator->() const { return get(); }
  T& operator*() const { return *get(); }
  [[nodiscard]] bool owns() const;
  std::unique_ptr<T> release();
};

template <typename T>
concept ScopedEnum = requires { std::is_scoped_enum<T>(); };

template <ScopedEnum>
class enumeration;

template <typename E>
concept Enumerable = DerivedFromTemplate<E, enumeration> && ScopedEnum<E>;

template <ScopedEnum E>
struct enumeration : public formattable {
  static_assert(std::formattable<E, char>);
  constexpr enumeration();
  constexpr enumeration& operator=(const enumeration&) = default;
  constexpr enumeration& operator=(enumeration&&)      = default;
  constexpr enumeration(enumeration&&)                 = default;
  constexpr enumeration(const enumeration&)            = default;
  constexpr enumeration(E e)
      : m_value(e) {};
  constexpr ~enumeration() override = default;

  // Conversion
  constexpr operator E() const { return inner(); };
  constexpr operator uint8_t() const { return (uint8_t)value(); };
  template <typename To>
  To cast() const;

  // Data
  static constexpr auto type_name() noexcept;
  static constexpr auto count() noexcept { return magic_enum::enum_count<E>(); }
  [[nodiscard]] constexpr auto name() const noexcept;
  [[nodiscard]] constexpr E inner() const noexcept { return m_value; }
  [[nodiscard]] constexpr auto value() const noexcept;

  [[nodiscard]] std::string format() const override;

  using value_type = E;

protected:
  E m_value;
};

template <typename T, typename V, auto N>
class enum_property : public formattable {
public:
  constexpr enum_property(T::value_type);
  constexpr enum_property()                                = default;
  constexpr ~enum_property() override                      = default;
  constexpr enum_property(const enum_property&)            = default;
  constexpr enum_property& operator=(const enum_property&) = default;
  constexpr enum_property(enum_property&&)                 = default;
  constexpr enum_property& operator=(enum_property&&)      = default;

  constexpr const V& read() const;
  constexpr operator const V&() const;

  [[nodiscard]] std::string format() const override;

private:
  V m_value;
};

#define ENUM_PROPERTY(TYPE, NAME, N) TYPE NAME

// Helper macro to extract types (every odd-positioned argument: 1st, 3rd, 5th,
// etc.)
#define GET_TYPES_1(t1, ...)                                 t1
#define GET_TYPES_2(t1, n1, t2, ...)                         t1, t2
#define GET_TYPES_3(t1, n1, t2, n2, t3, ...)                 t1, t2, t3
#define GET_TYPES_4(t1, n1, t2, n2, t3, n3, t4, ...)         t1, t2, t3, t4
#define GET_TYPES_5(t1, n1, t2, n2, t3, n3, t4, n4, t5, ...) t1, t2, t3, t4, t5
#define GET_TYPES_6(t1, n1, t2, n2, t3, n3, t4, n4, t5, n5, t6, ...) \
  t1, t2, t3, t4, t5, t6

// Helper macro to declare variables
#define DECLARE_VARS_2(t1, n1) ENUM_PROPERTY(t1, n1, 0);

#define DECLARE_VARS_4(t1, n1, t2, n2) \
  ENUM_PROPERTY(t1, n1, 0); \
  ENUM_PROPERTY(t2, n2, 1);
#define DECLARE_VARS_6(t1, n1, t2, n2, t3, n3) \
  ENUM_PROPERTY(t1, n1, 0); \
  ENUM_PROPERTY(t2, n2, 1); \
  ENUM_PROPERTY(t3, n3, 2);
#define DECLARE_VARS_8(t1, n1, t2, n2, t3, n3, t4, n4) \
  ENUM_PROPERTY(t1, n1, 0); \
  ENUM_PROPERTY(t2, n2, 1); \
  ENUM_PROPERTY(t3, n3, 2); \
  ENUM_PROPERTY(t4, n4, 3);

#define CTOR_PARAMS_2(t1, n1)                 t1 _##n1
#define CTOR_PARAMS_4(t1, n1, t2, n2)         t1 _##n1, t2 _##n2
#define CTOR_PARAMS_6(t1, n1, t2, n2, t3, n3) t1 _##n1, t2 _##n2, t3 _##n3
#define CTOR_PARAMS_8(t1, n1, t2, n2, t3, n3, t4, n4) \
  t1 _##n1, t2 _##n2, t3 _##n3, t4 _##n4

#define GET_VALUE(N)                  std::get<N>(element_type::properties_array().at(m_value))
#define CTOR_ASSIGN_2(t1, n1)         n1(GET_VALUE(0))
#define CTOR_ASSIGN_4(t1, n1, t2, n2) n1(GET_VALUE(0)), n2(GET_VALUE(1))
#define CTOR_ASSIGN_6(t1, n1, t2, n2, t3, n3) \
  n1(GET_VALUE(0)), n2(GET_VALUE(1)), n3(GET_VALUE(2))
#define CTOR_ASSIGN_8(t1, n1, t2, n2, t3, n3, t4, n4) \
  n1(GET_VALUE(0)), n2(GET_VALUE(1)), n3(GET_VALUE(2)), n4(GET_VALUE(3))

// Count arguments
#define GET_ARG_COUNT(...) \
  GET_ARG_COUNT_IMPL( \
      __VA_ARGS__, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
#define GET_ARG_COUNT_IMPL(_1, \
                           _2, \
                           _3, \
                           _4, \
                           _5, \
                           _6, \
                           _7, \
                           _8, \
                           _9, \
                           _10, \
                           _11, \
                           _12, \
                           _13, \
                           _14, \
                           _15, \
                           _16, \
                           N, \
                           ...) \
  N

// Macro concatenation helpers
#define CONCAT(a, b)      CONCAT_IMPL(a, b)
#define CONCAT_IMPL(a, b) a##b

// Additional helper for PPCAT
#define PPCAT(a, b)      PPCAT_IMPL(a, b)
#define PPCAT_IMPL(a, b) a##b

#define CONSTRUCT_VARS_2(t1, n1) , n1()

// Dispatch macros
#define DECLARE_VARS(...) \
  CONCAT(DECLARE_VARS_, GET_ARG_COUNT(__VA_ARGS__))(__VA_ARGS__)
#define GET_TYPES(...) \
  CONCAT(GET_TYPES_, GET_PAIR_COUNT(__VA_ARGS__))(__VA_ARGS__)
#define CTOR_PARAMS(...) \
  CONCAT(CTOR_PARAMS_, GET_ARG_COUNT(__VA_ARGS__))(__VA_ARGS__)
#define CTOR_ASSIGN(...) \
  CONCAT(CTOR_ASSIGN_, GET_ARG_COUNT(__VA_ARGS__))(__VA_ARGS__)

// Calculate number of pairs (divide arg count by 2)
#define GET_PAIR_COUNT(...)    GET_PAIR_COUNT_IMPL(GET_ARG_COUNT(__VA_ARGS__))
#define GET_PAIR_COUNT_IMPL(n) CONCAT(PAIR_COUNT_, n)
#define PAIR_COUNT_2           1
#define PAIR_COUNT_4           2
#define PAIR_COUNT_6           3
#define PAIR_COUNT_8           4
#define PAIR_COUNT_10          5
#define PAIR_COUNT_12          6
#define PAIR_COUNT_14          7
#define PAIR_COUNT_16          8

#define BUILD_ENUMERATION(TYPE, ...) \
  using value_type   = PPCAT(_, TYPE); \
  using element_type = TYPE; \
  using enumeration<value_type>::enumeration; \
  using enum value_type; \
  DECLARE_VARS(__VA_ARGS__) \
  using member_types = std::tuple<GET_TYPES(__VA_ARGS__)>; \
  using properties_map = \
      magic_enum::containers::array<value_type, member_types>; \
  [[nodiscard]] static constexpr const properties_map& properties_array(); \
  constexpr TYPE(value_type e) \
      : enumeration<value_type>(e), \
        CTOR_ASSIGN(__VA_ARGS__) {}

#define BUILD_ENUMERATION_CLASS(TYPE, ...) \
  struct TYPE : public cmm::enumeration<PPCAT(_, TYPE)> { \
    BUILD_ENUMERATION(TYPE, __VA_ARGS__) \
  };

template <typename T, template <typename> class BaseTemplate>
concept DerivedFromTemplate =
    std::is_base_of_v<BaseTemplate<typename T::value_type>, T>;

template <typename T> class stack {
public:
  using value_type                   = T;
  using pointer_type                 = T*;
  using const_pointer_type           = const T*;
  using reference_type               = T&;
  using const_reference_type         = const T&;
  using iterator                     = std::vector<T>::reverse_iterator;
  using reverse_iterator             = std::vector<T>::iterator;
  using const_iterator               = std::vector<T>::const_reverse_iterator;
  using const_reverse_iterator       = std::vector<T>::const_iterator;

  stack()                            = default;
  ~stack()                           = default;
  stack(const stack&)                = default;
  stack(stack&&) noexcept            = default;
  stack& operator=(const stack&)     = default;
  stack& operator=(stack&&) noexcept = default;

  bool operator==(const stack& other) const noexcept;
  bool operator!=(const stack& other) const noexcept;

  iterator begin() noexcept;
  iterator end() noexcept;
  [[nodiscard]] const_iterator begin() const noexcept;
  [[nodiscard]] const_iterator end() const noexcept;
  [[nodiscard]] const_iterator cbegin() const noexcept;
  [[nodiscard]] const_iterator cend() const noexcept;
  [[nodiscard]] const_reverse_iterator crbegin() const noexcept;
  [[nodiscard]] const_reverse_iterator crend() const noexcept;

  T& top() noexcept;
  [[nodiscard]] const T& top() const noexcept;
  template <typename Func> bool contains(Func&& func) const;
  template <typename Func> const T& find(Func func) const;
  template <typename Func> std::optional<size_t> find_position(Func func) const;
  template <typename Func> const_iterator find_all(Func func) const;
  template <typename Func> size_t count(Func func) const;
  template <typename... Args> void emplace_back(Args&&... args)
    requires std::is_constructible_v<T, Args...>;
  void push(const T& t) noexcept;
  void push(T&& t) noexcept;
  void pop();
  T pop_return();
  void clear() noexcept;
  void swap(stack& other) noexcept;
  [[nodiscard]] constexpr size_t size() const noexcept;
  [[nodiscard]] constexpr size_t max_size() const noexcept;
  [[nodiscard]] constexpr bool empty() const noexcept;

protected:
  std::vector<T> m_data;
};

} // namespace cmm

#include "stl.inl"
