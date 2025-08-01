#pragma once

#include "common.hpp"
#include "macros.hpp"

#include <array>
#include <cstdint>
#include <libassert/assert.hpp>
#include <magic_enum/magic_enum.hpp>
#include <type_traits>

#define NOT_IMPLEMENTED UNREACHABLE("Not implemented method");

namespace cmm {

template <typename... Args>
size_t hash_combine(const Args&... args) {
  size_t seed = 0;
  ((seed ^= typeid(args).hash_code() + 0x9e3779b9 + (seed << 6) + (seed >> 2)), ...);
  return seed;
}

enum class _type_t : uint8_t {
  base_t,
  fundamental_t,
  void_t,
  nullptr_t,
  arithmetic_t,
  integral_t,
  bool_t,
  char_t,
  uint_t,
  sint_t,
  float_t,
  compound_t,
  indirection_t,
  reference_t,
  lvalue_ref_t,
  rvalue_ref_t,
  pointer_t,
  array_t,
  function_t,
  enum_t,
  scoped_enum_t,
  unscoped_enum_t,
  class_t
};

enum class category_t : uint8_t {
  void_t,
  nullptr_t,
  bool_t,
  char_t,
  uint_t,
  sint_t,
  float_t,
  lvalue_ref_t,
  rvalue_ref_t,
  pointer_t,
  array_t,
  function_t,
  scoped_enum_t,
  unscoped_enum_t,
  class_t
};

using children_t = std::array<_type_t, 5>;

BUILD_ENUMERATION_CLASS(type_t,
                        _type_t,
                        self,
                        std::string_view,
                        format_str,
                        _type_t,
                        parent,
                        children_t,
                        children);

#define GROUP_TYPES \
  type_t::fundamental_t, type_t::void_t, type_t::arithmetic_t, type_t::integral_t, \
      type_t::compound_t, type_t::indirection_t, type_t::reference_t, type_t::enum_t,

#define INSTANCIABLE_TYPES() \
  type_t::nullptr_t, type_t::bool_t, type_t::char_t, type_t::uint_t, type_t::sint_t, \
      type_t::float_t, type_t::lvalue_ref_t, type_t::rvalue_ref_t, type_t::pointer_t, \
      type_t::arrayinstance_data_t, type_t::function_t, type_t::scoped_enum_t, \
      type_t::unscoped_enum_t, type_t::class_t

struct type_info {
  size_t value;
  type_info(size_t);
};

struct type : public formattable {
  category_t category;
  uint8_t rank                           = 0;
  const type* underlying                 = nullptr;
  bool c                                 = false;
  bool v                                 = false;

  constexpr type(const type&)            = default;
  constexpr type& operator=(const type&) = default;
  constexpr type(type&&)                 = default;
  constexpr type& operator=(type&&)      = default;
  constexpr ~type() override             = default;
  constexpr type(category_t t)
      : category(t) {}
  constexpr type(category_t t, bool c, bool v)
      : category(t),
        c(c),
        v(v) {}
  constexpr type(category_t t, const type& u)
      : category(t),
        underlying(&u) {}
  constexpr type(category_t t, const type& u, bool c, bool v)
      : category(t),
        underlying(&u),
        c(c),
        v(v) {}

  [[nodiscard]] constexpr std::string format() const override;
};

struct type_matcher {};

struct type_factory {
  STATIC_CLS(type_factory);

  static constexpr type create_fundamental(category_t, bool = false, bool = false);
  static constexpr type create_pointer(const type&, bool, bool);
  static constexpr type create_lvalue(const type&, bool, bool);
};

struct type_index {
  type_t type;
  size_t value;

  type_index(type_t, size_t);
};

#define OVERRIDE_HASHABLE(TYPE) \
  std::type_index type() const override { return typeid(TYPE); } \
  size_t hash() const override { return std::hash<std::string_view>()(type().name()); } \
  bool operator==(const TYPE&) const = default;

struct is_const_v {
  constexpr static bool operator()(const type&);
};

#define LVALUE_STATIC_TYPE(NAME, TYPE, CONST_, VOLATILE_, ...) \
  constexpr static type CONCAT(NAME, _T) = type(category_t::lvalue_ref_t, TYPE, false, false);

#define STATIC_TYPE(NAME, CLS, CONST_, VOLATILE_) \
  constexpr static type CONCAT(NAME, _T) = type(category_t::CLS, CONST_, VOLATILE_);

STATIC_TYPE(VOID, void_t, false, false);
STATIC_TYPE(UINT, uint_t, false, false);
STATIC_TYPE(INT, sint_t, false, false);
STATIC_TYPE(CHAR, char_t, false, false);
STATIC_TYPE(FLOAT, float_t, false, false);
// INDIR_STATIC_TYPE(INTPTR, pointer_t, INT_T, false, false)
LVALUE_STATIC_TYPE(INTREF, INT_T, false, false);

enum class mask_t : uint8_t {
  TYPE,
  REFERENCED_TYPE,
};

struct type_mask {
  static bool category(category_t);
};

// Implicit conversion from lvalue of int to int and vice versa
struct conversion {
  type from;
  type to;
};

struct conversions {
  std::vector<type> get_convertibles(const type&);
  bool is_convertible(const type&, const type&);
};
} // namespace cmm

#include "types.inl"
