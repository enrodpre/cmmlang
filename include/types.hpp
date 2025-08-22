#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <libassert/assert-macros.hpp>
#include <libassert/assert.hpp>
#include <magic_enum/magic_enum.hpp>
#include <magic_enum/magic_enum_containers.hpp>
#include <string>
#include <type_traits>
#include <typeinfo>
#include <utility>

#include "common.hpp"
#include "macros.hpp"

#define NOT_IMPLEMENTED UNREACHABLE("Not implemented method");

namespace cmm {

template <typename... Args> size_t hash_combine(const Args&... args) {
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
  class_t,
  matcher_t,
  dummy_t,
  generic_t,
  placeholder_t
};

using children_t = std::array<type_category_t, 4>;

BUILD_ENUMERATION_DATA_CLASS(type_category, type_category_t, parent, children_t, children);

static_assert(std::is_default_constructible_v<type_category_t>);

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

struct type_specifier : displayable {
  type_specifier(type_category_t);
  [[nodiscard]] virtual bool match(const type_specifier&) const = 0;
  friend bool operator==(const type_specifier& lhs, const type_specifier& rhs) {
    return lhs.match(rhs);
  }

  type_category_t category;
};

using ptype_spec = std::shared_ptr<type_specifier>;
using ctype_spec = const type_specifier&;

struct type : public type_specifier, cloneable<type> {
  uint8_t rank                               = 0;
  std::shared_ptr<type> underlying           = nullptr;
  bool c                                     = false;
  bool v                                     = false;

  constexpr ~type() override                 = default;
  constexpr type(const type&)                = default;
  constexpr type& operator=(const type&)     = default;
  constexpr type(type&&) noexcept            = default;
  constexpr type& operator=(type&&) noexcept = default;

protected:
  constexpr explicit type(type_category_t t)
      : type_specifier(t) {}
  constexpr type(type_category_t t, bool c, bool v)
      : type_specifier(t),
        c(c),
        v(v) {}
  constexpr type(type_category_t t, decltype(underlying) u)
      : type_specifier(t),
        underlying(std::move(u)) {}
  constexpr type(type_category_t t, decltype(underlying) u, bool c, bool v)
      : type_specifier(t),
        underlying(std::move(u)),
        c(c),
        v(v) {}
  constexpr type(type_category_t t, decltype(underlying) u, size_t n, bool c, bool v)
      : type_specifier(t),
        underlying(std::move(u)),
        rank(n),
        c(c),
        v(v) {}

public:
  [[nodiscard]] bool match(const type_specifier& other) const override {
    if (const auto* t = dynamic_cast<const type*>(&other)) {
      return *this == *t; // type vs type
    }
    return other.match(*this);
  }

  friend bool operator==(const type& lhs, const type& rhs) {
    return lhs.category == rhs.category && lhs.rank == rhs.rank &&
           lhs.underlying == rhs.underlying && lhs.c == rhs.c && lhs.v == rhs.v;
  }

  [[nodiscard]] type clone() const override;
  [[nodiscard]] std::string string() const override;
  template <typename... Args>
  static const type& create(type_category_t, Args&&...);
  static const type& create_void();
  static const type& create_fundamental(type_category_t, bool = false, bool = false);
  static const type& create_pointer(const type*, bool, bool);
  static const type& create_lvalue(const type*, bool = false, bool = false);
  static const type& create_array(const type*, size_t, bool, bool);
  static const type& create_string(size_t, bool = false, bool = false);
};

using ptype  = std::shared_ptr<type>;
using crtype = const type&;

struct generic_type : public type_specifier {
  generic_type()
      : type_specifier(type_category_t::generic_t) {}

  void resolve(ptype r) { resolved_type = std::move(r); };
  operator ptype() const {
    if (resolved_type != nullptr) {
      throw error("Type not resolved");
    }
    return resolved_type;
  }
  ptype resolved_type = nullptr;
};

#define LVALUE_STATIC_TYPE(NAME, TYPE, CONST_, VOLATILE_, ...) \
  static ptype const CONCAT(NAME, _T) = &type::create_lvalue(TYPE, false, false);

#define STATIC_TYPE(NAME, CLS, CONST_, VOLATILE_) \
  static ptype const CONCAT(NAME, _T) = \
      &type::create_fundamental(type_category_t::CLS, CONST_, VOLATILE_);

STATIC_TYPE(VOID, void_t, false, false);
STATIC_TYPE(NULLPTR, nullptr_t, false, false);
STATIC_TYPE(UINT, uint_t, false, false);
STATIC_TYPE(SINT, sint_t, false, false);
STATIC_TYPE(CHAR, char_t, false, false);
STATIC_TYPE(BOOL, bool_t, false, false);
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

template <type_category_t C> struct is_void : traits::false_type {};
template <> struct is_void<type_category_t::void_t> : traits::true_type {};

struct is_const_v {
  static constexpr bool operator()(crtype);
};
struct is_indirect_v {
  static constexpr bool operator()(crtype);
};
struct is_reference_v {
  static constexpr bool operator()(crtype);
};
bool belongs_to(const type_category_data&, type_category_t);

struct type_converter;
struct type_matcher : type_specifier {
  using value_type = std::function<bool(ptype)>;
  type_matcher(value_type&&);
  type_matcher(const value_type&);
  COPYABLE_CLS(type_matcher);
  bool match(const type_specifier&) const override;
  bool operator()(const type_specifier&) const;
  std::string string() const override;
  operator value_type() const;
  type_matcher operator&&(const type_matcher&) const;
  type_matcher operator||(const type_matcher&) const;
  type_matcher operator!() const;

protected:
  mutable ptype bound_type;

private:
  value_type m_matcher;
};

struct type_modifier {
  using modifier_t = std::function<> type_modifier operator|(const type_modifier&);
};

struct type_generic : type_specifier {};

struct type_dependant_placeholder;
struct type_placeholder : type_specifier {
  type_placeholder()
      : type_specifier(type_category_t::matcher_t),
        m_matcher([](ptype) { return true; }) {}
  type_placeholder(type_matcher);
  virtual std::shared_ptr<type_dependant_placeholder> create_dependant(type_matcher);
  bool match(const type_specifier&) const override;
  void resolve(ptype);
  std::string string() const override;

protected:
  type_matcher m_matcher;
  ptype m_resolved_type{};
  std::shared_ptr<type_dependant_placeholder> next_child;
};

struct type_dependant_placeholder : type_placeholder {
  type_dependant_placeholder(type_matcher);
  std::shared_ptr<type_dependant_placeholder> create_dependant(type_matcher) override;
};

namespace matchers {
extern const type_matcher is_arithmetic;
extern const type_matcher is_integral;
extern const type_matcher is_ref;
extern const type_matcher is_pointer;
extern const type_matcher is_floating;
extern const type_matcher is_unscoped;
extern const type_matcher is_array;
extern const type_matcher is_const;
extern const type_matcher is_cref;
extern const type_matcher is_volatile;
extern const type_matcher any;
} // namespace matchers

struct type_converter : type_modifier, displayable {
  struct builder;
  using type_converter_t = std::function<ptype(ptype)>;

  ptype operator()(ptype) const;
  bool is_convertible(ptype) const;
  [[nodiscard]] std::string string() const override { return m_desc; }

  friend builder;

private:
  type_converter();

  std::unique_ptr<type_specifier> m_from;
  type_converter_t m_converter;
  std::string m_desc;
};

struct type_converter::builder {
  builder& name(std::string);
  builder& from(const type_matcher&);
  builder& from(ptype);
  builder& to(ptype);
  builder& with(type_converter::type_converter_t);
  type_converter build();

private:
  type_converter m_obj;
};

namespace conversions {

extern const type_converter identity;
extern const type_converter nullptr_to_ptr;
extern const type_converter to_bool;
extern const type_converter bool_to_any;
extern const type_converter array_to_pointer;
extern const type_converter lvalue_to_rvalue;
extern const type_converter const_lvalue;

extern const std::array<const type_converter*, 6> standard;

}; // namespace conversions

extern std::vector<ptype> get_convertible_types(ptype);
extern bool is_convertible(ptype, ptype);

} // namespace cmm
template <> struct std::hash<cmm::type> {
  size_t operator()(const cmm::type& t) const noexcept {
    return cmm::hash_combine(t.category, t.rank, t.underlying, t.c, t.v);
  }
};

#include "types.inl"
