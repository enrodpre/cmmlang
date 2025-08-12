#pragma once

#include "common.hpp"
#include "macros.hpp"

#include <array>
#include <cstdint>
#include <libassert/assert.hpp>
#include <magic_enum/magic_enum.hpp>
#include <magic_enum/magic_enum_containers.hpp>

#define NOT_IMPLEMENTED UNREACHABLE("Not implemented method");

namespace cmm {

template <typename... Args>
size_t hash_combine(const Args&... args) {
  size_t seed = 0;
  ((seed ^= typeid(args).hash_code() + 0x9e3779b9 + (seed << 6) + (seed >> 2)), ...);
  return seed;
}

enum class type_category_t : uint8_t {
  any_t = 0,
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

static_assert(std::is_default_constructible_v<type_category_t>);

using type_metadata_t =
    std::tuple<type_category_t, type_category_t, std::array<type_category_t, 4>>;

using type_metadata_store_t =
    std::array<type_metadata_t, magic_enum::enum_count<type_category_t>()>;

extern const type_metadata_store_t type_metadata_store;

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
  type_category_t category;
  uint8_t rank                               = 0;
  const type* underlying                     = nullptr;
  bool c                                     = false;
  bool v                                     = false;

  constexpr ~type() override                 = default;
  constexpr type(const type&)                = delete;
  constexpr type& operator=(const type&)     = delete;
  constexpr type(type&&) noexcept            = default;
  constexpr type& operator=(type&&) noexcept = default;

private:
  constexpr type(type_category_t t)
      : category(t) {}
  constexpr type(type_category_t t, bool c, bool v)
      : category(t),
        c(c),
        v(v) {}
  constexpr type(type_category_t t, const type* u)
      : category(t),
        underlying(u) {}
  constexpr type(type_category_t t, const type* u, bool c, bool v)
      : category(t),
        underlying(u),
        c(c),
        v(v) {}
  constexpr type(type_category_t t, const type* u, size_t n, bool c, bool v)
      : category(t),
        underlying(u),
        rank(n),
        c(c),
        v(v) {}

public:
  bool operator==(const type& other) const {
    return category == other.category && rank == other.rank && *underlying == *other.underlying &&
           c == other.c && v == other.v;
  }

  [[nodiscard]] std::string format() const override;
  template <typename... Args>
  static const type& create(type_category_t, Args&&...);
  static const type& create_fundamental(type_category_t, bool = false, bool = false);
  static const type& create_pointer(const type*, bool, bool);
  static const type& create_lvalue(const type*, bool, bool);
  static const type& create_array(const type*, size_t, bool, bool);
  static const type& create_string(size_t, bool = false, bool = false);
};

using ptr_type = const type*;
using cr_type  = const type&;

struct type_matcher {};

struct type_index {
  type_category_t type;
  size_t value;

  type_index(type_category_t, size_t);
};

#define LVALUE_STATIC_TYPE(NAME, TYPE, CONST_, VOLATILE_, ...) \
  static const ptr_type CONCAT(NAME, _T) = &type::create_lvalue(TYPE, false, false);

#define STATIC_TYPE(NAME, CLS, CONST_, VOLATILE_) \
  static const ptr_type CONCAT(NAME, _T) = \
      &type::create_fundamental(type_category_t::CLS, CONST_, VOLATILE_);

STATIC_TYPE(VOID, void_t, false, false);
STATIC_TYPE(UINT, uint_t, false, false);
STATIC_TYPE(SINT, sint_t, false, false);
STATIC_TYPE(CHAR, char_t, false, false);
STATIC_TYPE(FLOAT, float_t, false, false);
// INDIR_STATIC_TYPE(INTPTR, pointer_t, INT_T, false, false)
LVALUE_STATIC_TYPE(SINTREF, SINT_T, false, false);

enum class mask_t : uint8_t {
  TYPE,
  REFERENCED_TYPE,
};

struct type_mask {
  static bool category(type_category_t);
};

// Implicit conversion from lvalue of int to int and vice versa

template <typename T>
struct result {
  result()
      : ok(false) {}
  result(const T& t)
      : ok(true),
        data(t) {}
  bool ok;
  T data;
  operator bool() const { return ok; }
};
result<ptr_type> is_assignable(cr_type from, cr_type to);

namespace types {
  template <type_category_t C>
  struct is_void : traits::false_type {};
  template <>
  struct is_void<type_category_t::void_t> : traits::true_type {};

  struct is_const_v {
    static constexpr bool operator()(cr_type);
  };
  struct is_indirect_v {
    static constexpr bool operator()(cr_type);
  };
  struct is_reference_v {
    static constexpr bool operator()(cr_type);
  };
  type_metadata_t get_metadata_of(type_category_t);
  // std::vector<type_category_t> all_children_of(type_category_t);
  bool belongs_to(type_category_t, type_category_t);
  bool belongs_to(cr_type, type_category_t);
}; // namespace types
} // namespace cmm

template <>
struct std::hash<cmm::type> {
  size_t operator()(const cmm::type& t) const noexcept {
    return cmm::hash_combine(t.category, t.rank, t.underlying, t.c, t.v);
  }
};
#include "types.inl"
