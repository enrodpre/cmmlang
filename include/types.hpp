#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <functional>

#include <magic_enum/magic_enum.hpp>
#include <magic_enum/magic_enum_containers.hpp>
#include <memory>
#include <string>
#include <type_traits>
#include <typeinfo>
#include <utility>

#include "common.hpp"
#include "macros.hpp"

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
  class_t,
  dummy_t,
  generic_t
};

using children_t = std::array<type_category_t, 4>;

BUILD_ENUMERATION_DATA_CLASS(type_category, type_category_t, parent, children_t, children);

static_assert(std::is_default_constructible_v<type_category_t>);

#define GROUP_TYPES                                                                   \
  type_t::fundamental_t, type_t::void_t, type_t::arithmetic_t, type_t::integral_t,    \
      type_t::compound_t, type_t::indirection_t, type_t::reference_t, type_t::enum_t,

#define INSTANCIABLE_TYPES()                                                          \
  type_t::nullptr_t, type_t::bool_t, type_t::char_t, type_t::uint_t, type_t::sint_t,  \
      type_t::float_t, type_t::lvalue_ref_t, type_t::rvalue_ref_t, type_t::pointer_t, \
      type_t::arrayinstance_data_t, type_t::function_t, type_t::scoped_enum_t,        \
      type_t::unscoped_enum_t, type_t::class_t

struct type;

using ptype   = std::shared_ptr<const type>;
using crptype = const ptype&;
using crtype  = const type&;

struct type : std::enable_shared_from_this<type>, displayable {
  type_category_t category;
  std::shared_ptr<const type> underlying = nullptr;
  uint8_t rank                           = 0;
  bool c                                 = false;
  bool v                                 = false;

  MOVABLE_CLS(type);
  COPYABLE_CLS(type);

  type(type_category_t t,
       decltype(underlying) u = nullptr,
       size_t n               = 0,
       bool c_                = false,
       bool v_                = false)
      : category(t),
        underlying(std::move(u)),
        rank(n),
        c(c_),
        v(v_) {}

  [[nodiscard]] virtual bool match(crptype other) const {
    if (!other) {
      return false;
    }
    if (typeid(this) == typeid(other.get())) {
      return category == other->category && rank == other->rank &&
             underlying == other->underlying && c == other->c && v == other->v;
    }
    return other->match(shared_from_this());
  }

  type clone() const { return {category, underlying, rank, c, v}; }
  [[nodiscard]] std::string string() const override;
  template <typename... Args>
  [[nodiscard]] static ptype create(type_category_t, Args&&...);
  [[nodiscard]] static ptype create_void();
  [[nodiscard]] static ptype create_fundamental(type_category_t, bool = false, bool = false);
  [[nodiscard]] static ptype create_pointer(ptype, bool = false, bool = false);
  [[nodiscard]] static ptype create_lvalue(ptype, bool = false, bool = false);
  [[nodiscard]] static ptype create_rvalue(ptype, bool = false, bool = false);
  [[nodiscard]] static ptype create_array(ptype, size_t, bool = false, bool = false);
  [[nodiscard]] static ptype create_string(size_t, bool = false, bool = false);
};

#define LVALUE_STATIC_TYPE(NAME, TYPE, CONST_, VOLATILE_, ...)                   \
  static const ptype CONCAT(NAME, _T) = type::create_lvalue(TYPE, false, false);

#define STATIC_TYPE(NAME, CLS, CONST_, VOLATILE_)                        \
  static const ptype CONCAT(NAME, _T) =                                  \
      type::create_fundamental(type_category_t::CLS, CONST_, VOLATILE_);

STATIC_TYPE(VOID, void_t, false, false);
STATIC_TYPE(NULLPTR, nullptr_t, false, false);
STATIC_TYPE(UINT, uint_t, false, false);
STATIC_TYPE(SINT, sint_t, false, false);
STATIC_TYPE(CHAR, char_t, false, false);
STATIC_TYPE(BOOL, bool_t, false, false);
STATIC_TYPE(FLOAT, float_t, false, false);
// INDIR_STATIC_TYPE(INTPTR, pointer_t, INT_T, false, false)
LVALUE_STATIC_TYPE(SINTREF, SINT_T, false, false);
LVALUE_STATIC_TYPE(UINTREF, UINT_T, false, false);
LVALUE_STATIC_TYPE(FLOATREF, FLOAT_T, false, false);

enum class mask_t : uint8_t {
  TYPE,
  REFERENCED_TYPE,
};

bool belongs_to(const type_category_data&, type_category_t);

struct type_modifier : displayable {
  using modifier_t = std::function<ptype(crptype)>;
  type_modifier(std::string desc, modifier_t mod)
      : m_modifier(mod),
        m_desc(desc) {}
  std::string string() const override { return m_desc; }
  ptype operator()(crptype) const;
  type_modifier operator|(const type_modifier&) const;

protected:
  modifier_t m_modifier;
  std::string m_desc;
};

#define create_mod(NAME, RET) static const type_modifier NAME(#NAME, [](ptype t) -> ptype { RET; });
#define create_wrapper(NAME, CAT)                                                              \
  static const type_modifier NAME{#NAME, [](crptype t) -> ptype {                              \
                                    return type::create(                                       \
                                        type_category_t::CAT, t->underlying, 0, false, false); \
                                  }};
#define create_to_type(NAME, TYPE)                                      \
  static const type_modifier NAME(#NAME, [](crptype) { return TYPE; });

namespace modifiers {
create_mod(identity, return t);
create_mod(decay, return t);
create_mod(get_underlying, return t->underlying);
create_mod(
    remove_reference,
    if (belongs_to(t->category, type_category_t::reference_t)) { return t->underlying; } return t);
create_mod(add_const, auto cloned = t->clone(); cloned.c = true;
           return std::shared_ptr<const type>(&cloned));
create_wrapper(add_rvalue_reference, rvalue_ref_t);
create_wrapper(add_lvalue_reference, lvalue_ref_t);
create_wrapper(add_pointer, pointer_t);
}; // namespace modifiers

using match_t   = std::function<bool(crtype)>;
using compare_t = std::function<bool(crtype, crtype)>;

struct type_matcher : public type {
  ~type_matcher() { registers[m_desc].second++; }
  using value_type = std::function<bool(crptype)>;
  type_matcher(std::string desc, value_type v)
      : type(type_category_t::generic_t),
        m_matcher(std::move(v)),
        m_desc(std::move(desc)) {
    registers[m_desc].first++;
  }
  type_matcher(std::string desc, const type_matcher& v)
      : type(type_category_t::generic_t),
        m_matcher(std::move(v)),
        m_desc(std::move(desc)) {
    registers[m_desc].first++;
  }
  COPYABLE_CLS(type_matcher);
  NOT_MOVABLE_CLS(type_matcher);
  bool operator()(crptype) const;
  [[nodiscard]] bool match(crptype) const override;
  [[nodiscard]] std::string string() const override;
  type_matcher operator&&(const type_matcher&) const;
  type_matcher operator||(const type_matcher&) const;
  type_matcher operator!() const;
  friend bool operator==(const type_matcher& lhs, const type_matcher& rhs) {
    return lhs.string() == rhs.string();
  }
  inline static std::unordered_map<std::string, std::pair<size_t, size_t>> registers{};

private:
  value_type m_matcher;
  std::string m_desc;
};

inline bool operator==(crptype lhs, crptype rhs) {
  // Handle null pointer cases first
  if (!lhs && !rhs)
    return true;
  if (!lhs || !rhs)
    return false;

  // If same object, return true
  if (lhs.get() == rhs.get())
    return true;

  // Use virtual equals method for polymorphic comparison
  return lhs->match(rhs);
}

#define make_belongs(NAME, CAT)                                                                    \
  inline static const type_matcher NAME(                                                           \
      #NAME, [](crptype t) -> bool { return t && belongs_to(t->category, type_category_t::CAT); })

#define make_is(NAME, CAT)                                                               \
  inline static const type_matcher NAME(                                                 \
      #NAME, [](crptype t) -> bool { return t && type_category_t::CAT == t->category; })

#define make_matcher(NAME, COND)                                                           \
  inline static const type_matcher NAME(#NAME, [](crptype lhs) -> bool { return (COND); })

#define make_combined(NAME, COMBINED) inline static const type_matcher NAME(#NAME, COMBINED)

namespace matchers {
inline static const type_matcher any("Any", [](crptype) { return true; });
make_belongs(is_arithmetic, arithmetic_t);
make_belongs(is_integral, integral_t);
make_is(is_lvalue, lvalue_ref_t);
make_is(is_rvalue, rvalue_ref_t);
make_combined(is_ref, is_lvalue || is_rvalue);
make_is(is_pointer, pointer_t);
make_is(is_floating, float_t);
make_is(is_unscoped, unscoped_enum_t);
make_is(is_array, array_t);
make_matcher(is_const, lhs->c);
make_matcher(is_volatile, lhs->v);
make_combined(is_const_lvalue, is_const&& is_lvalue);

} // namespace matchers

struct type_converter : displayable {
  struct builder;

  ptype operator()(ptype) const;
  [[nodiscard]] bool is_convertible(const ptype&) const;

  [[nodiscard]] std::string string() const override { return m_desc; }

  friend builder;

private:
  type_converter() = default;

  ptype m_from;
  type_modifier m_converter = modifiers::identity;
  std::string m_desc;
};

struct type_comparer {};

DECLARE_BUILDER(type_converter)
BUILDER_STEP(name, std::string, m_desc)
BUILDER_STEP_SETTER(from, type_matcher, m_from, std::make_shared<const type_matcher>(value))
BUILDER_STEP(from, ptype, m_from)
BUILDER_STEP_SETTER(to,
                    ptype,
                    m_converter,
                    type_modifier(std::format("to {}", value), [value](crptype) { return value; }))
BUILDER_STEP(with, type_modifier, m_converter)
END_BUILDER()

#define BINDING_CONVERSION(NAME, COND, WITH)                                                    \
  inline const auto& NAME = type_converter::builder().name(#NAME).from(COND).with(WITH).build()
#define NORMAL_CONVERSION(NAME, COND, TO)                                                   \
  inline const auto& NAME = type_converter::builder().name(#NAME).from(COND).to(TO).build()

namespace conversions {

BINDING_CONVERSION(lvalue_to_rvalue,
                   matchers::is_lvalue,
                   modifiers::remove_reference | modifiers::add_rvalue_reference);
// extern const type_converter nullptr_to_ptr;
NORMAL_CONVERSION(any_to_bool,
                  matchers::is_integral || matchers::is_pointer || matchers::is_unscoped ||
                      matchers::is_floating,
                  BOOL_T);
// extern const type_converter bool_to_any;
BINDING_CONVERSION(array_to_pointer, matchers::is_array, modifiers::decay);

inline const std::array standard = {&any_to_bool, &lvalue_to_rvalue};

}; // namespace conversions

extern std::vector<ptype> get_convertible_types(crptype);
extern bool is_convertible(crptype, crptype);

} // namespace cmm

template <>
struct std::hash<cmm::type> {
  size_t operator()(const cmm::type& t) const noexcept {
    return cmm::hash_combine(t.category, t.rank, t.underlying, t.c, t.v);
  }
};

#include "types.inl"
