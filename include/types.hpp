#pragma once

#include "common.hpp"
#include "macros.hpp"
#include <array>
#include <cstdint>
#include <memory>

namespace cmm {

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
  type_t::fundamental_t, type_t::void_t, type_t::arithmetic_t, \
      type_t::integral_t, type_t::compound_t, type_t::indirection_t, \
      type_t::reference_t, type_t::enum_t,

#define INSTANCIABLE_TYPES() \
  type_t::nullptr_t, type_t::bool_t, type_t::char_t, type_t::uint_t, \
      type_t::sint_t, type_t::float_t, type_t::lvalue_ref_t, \
      type_t::rvalue_ref_t, type_t::pointer_t, type_t::arrayinstance_data_t, \
      type_t::function_t, type_t::scoped_enum_t, type_t::unscoped_enum_t, \
      type_t::class_t

struct type_info {
  size_t value;
  type_info(size_t);
};

struct rtti;

struct type_index {
  type_t type;
  size_t value;

  type_index(type_t, size_t);
  [[nodiscard]] const rtti& get() const;
};

struct array_data {
  type_t element_t;
  size_t length;

  array_data(type_t, size_t, bool, bool);
};

struct enum_data {
  type_t underlying_t;
  size_t length;
};

struct indirect_data {
  type_index& obj;
};

#define OVERRIDE_HASHABLE(TYPE) \
  std::type_index type() const override { \
    return typeid(TYPE); \
  } \
  size_t hash() const override { \
    return std::hash<std::string_view>()(type().name()); \
  } \
  bool operator==(const TYPE&) const = default;

using instance_data =
    std::variant<std::monostate, array_data, enum_data, indirect_data>;

struct rtti : public formattable {
  const type_index identifier;
  const type_t type;
  bool c : 1;
  bool v : 1;
  instance_data data;

  rtti(type_t&&, instance_data&&, bool = false, bool = false);
  ~rtti() override = default;
  NOT_COPYABLE_CLS(rtti);
  NOT_MOVABLE_CLS(rtti);

  operator const type_index&() const;
  [[nodiscard]] std::string format() const override;
};

using cv_rtti = const rtti*;

struct is_const_v {
  constexpr static bool operator()(cv_rtti);
};

struct type_storage : default_singleton<type_storage> {
  template <typename... Args>
  const rtti* get(type_t, Args&&...) noexcept;
  template <typename... Args>
  const rtti* get(const std::type_index&) noexcept;

private:
  std::unordered_map<std::type_index, std::unique_ptr<rtti>> m_storage;
};

struct builder {
  STATIC_CLS(builder)

public:
  struct build_step {
    build_step& const_qual();
    build_step& volatile_qual();
    build_step& pointer_to();
    build_step& reference_of();
    template <typename... Args>
    rtti type_class(Args&&...);

  private:
    enum class step : uint8_t { const_ = 0, volatile_, pointer, reference };
    std::vector<step> steps;
  };

  static build_step init();

  template <typename T>
  constexpr static rtti& create(bool const_    = false,
                                bool volatile_ = false) noexcept;
  template <typename... Args>
  constexpr static rtti& create(type_t,
                                bool const_    = false,
                                bool volatile_ = false,
                                Args&&...);
};
} // namespace cmm
template <>
struct std::hash<cmm::rtti> {
  std::size_t operator()(const cmm::rtti& k) const {
    return ((hash<bool>()(k.c) ^
             (hash<bool>()(k.v) ^ (std::hash<cmm::_type_t>()(k.type)))));
  }
};

namespace cmm {

template <typename T = rtti>
using type_erased = std::unique_ptr<T>;

static inline type_storage type_store;

#define INDIR_STATIC_TYPE(NAME, CLS, TYPE, CONST_, VOLATILE_, ...) \
  const static inline auto CONCAT(NAME, _T) = \
      type_store.get(CLS, CONST_, VOLATILE_, TYPE);

#define STATIC_TYPE(NAME, CLS, CONST_, VOLATILE_) \
  const static inline auto CONCAT(NAME, _T) = \
      type_store.get(CLS, CONST_, VOLATILE_);

STATIC_TYPE(UINT, type_t::uint_t, false, false);
STATIC_TYPE(INT, type_t::sint_t, false, false);
STATIC_TYPE(CHAR, type_t::char_t, false, false);
STATIC_TYPE(FLOAT, type_t::float_t, false, false);
INDIR_STATIC_TYPE(INTPTR, type_t::pointer_t, INT_T, false, false)
INDIR_STATIC_TYPE(INTREF, type_t::lvalue_ref_t, INT_T, false, false)
} // namespace cmm

#include "types.inl"
