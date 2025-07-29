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

class type;
using cv_type = const type*;

class type : public formattable {
public:
  type(bool = false, bool = false);
  ~type() override = default;

  [[nodiscard]] std::string format() const noexcept override { return name(); }
  [[nodiscard]] virtual std::string name() const noexcept;

  [[nodiscard]] virtual bool is_same_as(cv_type) const noexcept;
  [[nodiscard]] bool is_const() const noexcept { return m_const; }
  [[nodiscard]] bool is_volatile() const noexcept { return m_volatile; }

  [[nodiscard]] virtual size_t alignment() const noexcept              = 0;
  [[nodiscard]] virtual cv_type underlying() const noexcept            = 0;
  [[nodiscard]] virtual size_t size() const noexcept                   = 0;
  [[nodiscard]] virtual size_t rank() const noexcept                   = 0;

  [[nodiscard]] virtual bool is_complete() const noexcept              = 0;
  [[nodiscard]] virtual bool is_scalar() const noexcept                = 0;
  [[nodiscard]] virtual bool is_compound() const noexcept              = 0;
  [[nodiscard]] virtual bool is_convertible_to(cv_type) const noexcept = 0;

protected:
  bool m_const : 1;
  bool m_volatile : 1;

private:
};

class fundamental_type : public type {
public:
  using type::type;
  [[nodiscard]] size_t alignment() const noexcept override { return 0; }
  [[nodiscard]] cv_type underlying() const noexcept override { return nullptr; }
  [[nodiscard]] size_t rank() const noexcept override { return 0; }

  [[nodiscard]] bool is_scalar() const noexcept override { return true; }
  [[nodiscard]] bool is_compound() const noexcept override { return false; }
  [[nodiscard]] bool is_convertible_to(cv_type) const noexcept override { NOT_IMPLEMENTED; }
};

class void_type final : public fundamental_type {
public:
  static type_t category() noexcept { return type_t::void_t; }
  void_type(bool, bool);
  [[nodiscard]] size_t size() const noexcept override { return 0; }

  [[nodiscard]] bool is_complete() const noexcept override { return false; }
  [[nodiscard]] bool is_convertible_to(cv_type) const noexcept override { NOT_IMPLEMENTED; }
};

class nullptr_type final : public fundamental_type {
public:
  using fundamental_type::fundamental_type;
  static type_t category() noexcept { return type_t::nullptr_t; }
  [[nodiscard]] bool is_complete() const noexcept override { return true; }
  [[nodiscard]] size_t size() const noexcept override { return MEM_ADDR_LEN; }
};
class arithmetic_type : public fundamental_type {
public:
  using fundamental_type::fundamental_type;
  [[nodiscard]] bool is_complete() const noexcept override { return true; };
};
class integral_type : public arithmetic_type {
  using arithmetic_type::arithmetic_type;
};
class bool_type final : public integral_type {
public:
  static type_t category() noexcept { return type_t::bool_t; }
  bool_type(bool, bool);
  [[nodiscard]] size_t size() const noexcept override { return 1; }
};
class char_type final : public integral_type {
public:
  static type_t category() noexcept { return type_t::char_t; }
  char_type(bool, bool);
  [[nodiscard]] size_t size() const noexcept override { return 1; }
};
class sint_type final : public integral_type {
public:
  static type_t category() noexcept { return type_t::sint_t; }
  sint_type(bool, bool);
  [[nodiscard]] size_t size() const noexcept override { return 4; }
};
class uint_type final : public integral_type {
public:
  static type_t category() noexcept { return type_t::uint_t; }
  uint_type(bool, bool);
  [[nodiscard]] size_t size() const noexcept override { return 4; }
};
class float_type final : public arithmetic_type {
public:
  static type_t category() noexcept { return type_t::float_t; }
  float_type(bool, bool);
  [[nodiscard]] size_t size() const noexcept override { return 4; }
};
class compound_type : public type {
public:
  compound_type(bool, bool, cv_type);
  [[nodiscard]] cv_type underlying() const noexcept override { return m_underlying; }
  [[nodiscard]] size_t alignment() const noexcept override { return 0; }

  [[nodiscard]] bool is_complete() const noexcept override { return true; };
  [[nodiscard]] bool is_scalar() const noexcept override { return false; }
  [[nodiscard]] bool is_compound() const noexcept override { return true; }
  [[nodiscard]] bool is_convertible_to(cv_type) const noexcept override { NOT_IMPLEMENTED; }

protected:
  cv_type m_underlying;
};
class reference_type : public compound_type {
public:
  using compound_type::compound_type;
  [[nodiscard]] size_t size() const noexcept override { return MEM_ADDR_LEN; }
  [[nodiscard]] size_t rank() const noexcept override { return 1; }
};
class lvalue_ref_type final : public reference_type {
public:
  lvalue_ref_type(bool, bool, cv_type);
  static type_t category() noexcept { return type_t::lvalue_ref_t; }
};
class rvalue_ref_type final : public reference_type {
public:
  rvalue_ref_type(bool, bool, cv_type);
  static type_t category() noexcept { return type_t::rvalue_ref_t; }
};
class pointer_type final : public compound_type {
public:
  pointer_type(bool, bool, cv_type);
  static type_t category() noexcept { return type_t::pointer_t; }
  [[nodiscard]] size_t size() const noexcept override { return MEM_ADDR_LEN; }
  [[nodiscard]] size_t rank() const noexcept override { return 1; }
};
class member_pointer_type final : public compound_type {
public:
  [[nodiscard]] size_t size() const noexcept override { return MEM_ADDR_LEN; }
  [[nodiscard]] size_t rank() const noexcept override { return 1; }
};
class array_type final : public compound_type {
public:
  static type_t category() noexcept { return type_t::array_t; }
  array_type(cv_type, size_t);
  [[nodiscard]] size_t size() const noexcept override { return m_rank * m_underlying->size(); }
  [[nodiscard]] size_t rank() const noexcept override { return m_rank; }

private:
  size_t m_rank;
};
class function_type final : public compound_type {
public:
  static type_t category() noexcept { return type_t::function_t; }
  [[nodiscard]] size_t size() const noexcept override { NOT_IMPLEMENTED; }
  [[nodiscard]] size_t rank() const noexcept override { NOT_IMPLEMENTED; }

private:
  // m_underlying is return type
  std::vector<cv_type> m_parameters;
};

class enum_type : public compound_type {
public:
  [[nodiscard]] size_t size() const noexcept override { return m_underlying->size() * rank(); }
  [[nodiscard]] size_t rank() const noexcept override { return m_elements.size(); }

private:
  std::vector<std::string> m_elements;
};
class unscoped_enum_type final : public enum_type {
public:
  static type_t category() noexcept { return type_t::unscoped_enum_t; }
};
class scoped_enum_type final : public enum_type {
public:
  static type_t category() noexcept { return type_t::scoped_enum_t; }
};
class class_type final : public compound_type {
public:
  static type_t category() noexcept { return type_t::class_t; }
  [[nodiscard]] size_t size() const noexcept override { NOT_IMPLEMENTED; }
  [[nodiscard]] size_t rank() const noexcept override { NOT_IMPLEMENTED; }
};

struct type_index {
  type_t type;
  size_t value;

  type_index(type_t, size_t);
  [[nodiscard]] cv_type get() const;
};

#define OVERRIDE_HASHABLE(TYPE) \
  std::type_index type() const override { return typeid(TYPE); } \
  size_t hash() const override { return std::hash<std::string_view>()(type().name()); } \
  bool operator==(const TYPE&) const = default;

struct is_const_v {
  constexpr static bool operator()(cv_type);
};

struct builder {
  STATIC_CLS(builder)

public:
  struct build_step {
    build_step& const_qual();
    build_step& volatile_qual();
    build_step& pointer_to();
    build_step& reference_of();
    template <typename... Args> cv_type type_class(Args&&...);

  private:
    enum class step : uint8_t { const_ = 0, volatile_, pointer, reference };
    std::vector<step> steps;
  };

  static build_step init();

  template <typename T>
  constexpr static cv_type create(bool const_ = false, bool volatile_ = false) noexcept;
  template <typename... Args>
  constexpr static cv_type create(type_t, bool const_ = false, bool volatile_ = false, Args&&...);
};

static inline struct {
  template <typename... Args> cv_type get(const std::type_index&) noexcept;

  template <typename T, typename... Args>
    requires(std::is_constructible_v<T, bool, bool, Args...> && std::is_base_of_v<type, T>)
  cv_type get(bool c, bool v, Args&&... args) {
    size_t hash_value = hash_combine(T::category(), c, v, std::forward<Args>(args)...);
    if (!contains(hash_value)) {
      m_storage.emplace(hash_value, std::make_unique<T>(c, v, std::forward<Args>(args)...));
    }
    return retrieve(T::category(), c, v, std::forward<Args>(args)...);
  }

  template <typename... Args>
  cv_type get(const type_t& t, Args&&... args) {
    switch (t.inner()) {
      case _type_t::bool_t:
        return get<bool_type>(std::forward<Args>(args)...);
      case _type_t::char_t:
        return get<char_type>(std::forward<Args>(args)...);
      case _type_t::uint_t:
        return get<uint_type>(std::forward<Args>(args)...);
      case _type_t::sint_t:
        return get<sint_type>(std::forward<Args>(args)...);
      case _type_t::float_t:
        return get<float_type>(std::forward<Args>(args)...);
      case _type_t::lvalue_ref_t:
        return get<rvalue_ref_type>(std::forward<Args>(args)...);
      case _type_t::rvalue_ref_t:
        return get<rvalue_ref_type>(std::forward<Args>(args)...);
      case _type_t::pointer_t:
        return get<pointer_type>(std::forward<Args>(args)...);
      case _type_t::array_t:
        return get<array_type>(std::forward<Args>(args)...);
      case _type_t::function_t:
        return get<function_type>(std::forward<Args>(args)...);
      case _type_t::scoped_enum_t:
        return get<scoped_enum_type>(std::forward<Args>(args)...);
      case _type_t::unscoped_enum_t:
        return get<unscoped_enum_type>(std::forward<Args>(args)...);
      case _type_t::class_t:
        return get<class_type>(std::forward<Args>(args)...);
      case _type_t::enum_t:
      case _type_t::compound_t:
      case _type_t::indirection_t:
      case _type_t::reference_t:
      case _type_t::base_t:
      case _type_t::fundamental_t:
      case _type_t::void_t:
      case _type_t::nullptr_t:
      case _type_t::arithmetic_t:
      case _type_t::integral_t:
        UNREACHABLE("Type not instanciable");
    }
  }

  template <typename... Args>
  cv_type retrieve(Args&&... args) const {
    return m_storage.at(hash_combine(std::forward<Args>(args)...)).get();
  }

private:
  template <typename... Args>
  bool contains(Args&&... args) const {
    return contains(hash_combine(std::forward<Args>(args)...));
  }

  bool contains(size_t hash_value) const { return m_storage.contains(hash_value); }

  template <typename T, typename... Args>
    requires(std::is_constructible_v<T, Args...> && std::is_base_of_v<type, T>)
  T create(Args&&... args) {
    size_t hash_value = hash_combine(std::forward<Args>(args)...);
  }

  std::unordered_map<size_t, std::unique_ptr<type>> m_storage;
} type_store;

#define INDIR_STATIC_TYPE(NAME, CLS, TYPE, CONST_, VOLATILE_, ...) \
  const static inline cv_type CONCAT(NAME, _T) = type_store.get<CLS>(CONST_, VOLATILE_, TYPE);

#define STATIC_TYPE(NAME, CLS, CONST_, VOLATILE_) \
  const static inline cv_type CONCAT(NAME, _T) = type_store.get<CLS>(CONST_, VOLATILE_);

STATIC_TYPE(UINT, uint_type, false, false);
STATIC_TYPE(INT, sint_type, false, false);
STATIC_TYPE(CHAR, char_type, false, false);
STATIC_TYPE(FLOAT, float_type, false, false);
INDIR_STATIC_TYPE(INTPTR, pointer_type, INT_T, false, false)
INDIR_STATIC_TYPE(INTREF, lvalue_ref_type, INT_T, false, false)
} // namespace cmm

#include "types.inl"
