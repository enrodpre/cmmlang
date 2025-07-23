#pragma once

#include "allocator.hpp"
#include "common.hpp"
#include "traits.hpp"
#include <cstddef>
#include <cstdint>
#include <fmt/base.h>
#include <format>
#include <magic_enum/magic_enum.hpp>
#include <magic_enum/magic_enum_format.hpp>
#include <type_traits>

#define DEFAULT_VALUE_OVERRIDE(value) \
  [[nodiscard]] cstring default_value() const override { \
    return value; \
  }

namespace cmm {

enum class _instruction_t : uint8_t {
  nop = 0,

  // Jumps
  jmp,
  je,
  jne,
  jz,
  jnz,
  jg,
  jge,
  jl,
  jle,

  // Management
  mov,
  lea,
  push,
  pop,

  // Comparison
  cmp,
  test,

  // Bitwise
  and_,
  or_,
  xor_,
  not_,
  inc,
  dec,

  // Arithmetics
  add,
  sub,
  mul,
  imul,
  div,
  idiv,

  // Misc
  syscall,
  ret,
  call,

  // Variables
  global,
};

enum class _operator_t : uint8_t {
  plus = 0,
  minus,
  star,
  fslash,

  pre_inc,
  pre_dec,
  post_inc,
  post_dec,

  // xor_b,
  // or_b,
  // and_b,
  // not_b,

  eq,
  neq,
  lt,
  le,
  gt,
  ge,

  xor_,
  or_,
  and_,
  not_,

  ampersand,
  assign,
};

enum class associativity_t : uint8_t { Either, L2R, R2L };

BUILD_ENUMERATION_CLASS(instruction_t,
                        short,
                        n_params,
                        bool,
                        can_address_memory,
                        std::optional<_instruction_t>,
                        inverse_jump);

struct operator_t : public cmm::enumeration<_operator_t> {
  BUILD_ENUMERATION(operator_t,
                    std::string,
                    repr,
                    uint8_t,
                    precedence,
                    associativity_t,
                    assoc,
                    std::optional<instruction_t>,
                    ins)

  [[nodiscard]] std::string caller_function() const;
  [[nodiscard]] std::string format() const override;
};

enum class type_t : uint8_t {
  void_t,
  nullptr_t,
  bool_t,
  char_t,
  byte_t,
  int_t,
  short_t,
  double_t,
  long_t,
  float_t,
  class_t,
  enum_t,
  struct_t,
};

enum class attribute : uint8_t {
  no_return          = 1 << 1,
  carries_dependency = 1 << 2,
  deprecated         = 1 << 3,
};

enum class linkage_t : uint8_t { normal = 0, internal, external };
enum class storage_t : uint8_t {
  normal = 0,
  static_,
  extern_,
  mutable_,
  register_
};

enum class modifier_t : uint8_t {
  friend_,
  constexpr_,
  const_,
  volatile_,
  ptr,
  ref,
  signed_,
  unsigned_,
  constinit_,
  consteval_,
};

namespace types {
  struct base_type;
  using type = const base_type*;

} // namespace types

using cv_type = const types::base_type*;
namespace types {
  struct builder;

  struct base_type : public formattable, public hashable {
    base_type() = default;
    base_type(bool c, bool v)
        : const_(c),
          volatile_(v) {}
    bool const_                                       = false;
    bool volatile_                                    = false;
    [[nodiscard]] std::string format() const override = 0;
    virtual type decay() { return this; }
    [[nodiscard]] virtual ushort size() const = 0;
    template <typename T, typename... Types>
    [[nodiscard]] bool is() const {
      // Base case: if Types... is empty, return false
      if constexpr (sizeof...(Types) == 0) {
        return dynamic_cast<const T*>(this) != nullptr;
      } else {
        // Recursive case: check T and then check Types...
        return dynamic_cast<const T*>(this) != nullptr || is<Types...>();
      }
    }

    bool operator==(const base_type& other) const;
    [[nodiscard]] virtual cstring default_value() const = 0;
    [[nodiscard]] bool is_const() const { return const_; }
    [[nodiscard]] bool is_volatile() const { return volatile_; }
    [[nodiscard]] virtual bool is_reference() const { return false; }
    [[nodiscard]] static auto tie() { return std::tie(); };
  };

  struct fundamental_t : public base_type {
    using base_type::base_type;
    [[nodiscard]] ushort size() const override = 0;
  };
  struct void_t : public fundamental_t {
    using fundamental_t::fundamental_t;
    [[nodiscard]] ushort size() const override { return 0; }
    [[nodiscard]] cstring default_value() const override { PANIC(); }
    [[nodiscard]] std::string format() const override { return "void"; }
  };
  struct nullptr_t : public fundamental_t {
    using fundamental_t::fundamental_t;
    [[nodiscard]] ushort size() const override { return 0; }
    [[nodiscard]] cstring default_value() const override { PANIC(); }
    [[nodiscard]] std::string format() const override { return "nullptr"; }
  };
  struct arithmetic_t : public fundamental_t {
    using fundamental_t::fundamental_t;
    [[nodiscard]] ushort size() const override = 0;
  };
  struct integral_t : public arithmetic_t {
    using arithmetic_t::arithmetic_t;
    [[nodiscard]] ushort size() const override = 0;
  };
  struct bool_t : public integral_t {
    using integral_t::integral_t;

    [[nodiscard]] ushort size() const override { return 1; }
    DEFAULT_VALUE_OVERRIDE("0")
    [[nodiscard]] std::string format() const override;
    [[nodiscard]] auto tie() const { return std::tie(const_, volatile_); };
  };
  struct char_t : public integral_t {
    using integral_t::integral_t;
    [[nodiscard]] ushort size() const override { return 1; }
    DEFAULT_VALUE_OVERRIDE("0")
    [[nodiscard]] std::string format() const override;
    [[nodiscard]] auto tie() const { return std::tie(const_, volatile_); };
  };
  struct sint_t : public integral_t {
    using integral_t::integral_t;
    [[nodiscard]] ushort size() const override { return 4; }
    DEFAULT_VALUE_OVERRIDE("0")
    [[nodiscard]] std::string format() const override;
    [[nodiscard]] auto tie() const { return std::tie(const_, volatile_); };
  };
  struct uint_t : public integral_t {
    using integral_t::integral_t;
    [[nodiscard]] ushort size() const override { return 4; }
    DEFAULT_VALUE_OVERRIDE("0")
    [[nodiscard]] std::string format() const override;
    [[nodiscard]] auto tie() const { return std::tie(const_, volatile_); };
  };
  struct float_t : public arithmetic_t {
    using arithmetic_t::arithmetic_t;
    [[nodiscard]] ushort size() const override { return WORD_LEN; }
    DEFAULT_VALUE_OVERRIDE("0")
    [[nodiscard]] std::string format() const override;
    [[nodiscard]] auto tie() const { return std::tie(const_, volatile_); };
  };

  struct compound_t : public base_type {
    using base_type::base_type;
    [[nodiscard]] ushort size() const override = 0;
  };
  struct indirection_t : public compound_t {
    type type_;
    indirection_t(bool c, bool v, type ptr)
        : compound_t(c, v),
          type_(ptr) {}
    bool operator==(const indirection_t& other) const;
    [[nodiscard]] ushort size() const override { return WORD_LEN; }
    [[nodiscard]] auto tie() const {
      return std::tie(const_, volatile_, type_);
    };
  };
  struct reference_t : public indirection_t {
    using indirection_t::indirection_t;
    [[nodiscard]] ushort size() const override { return MEM_ADDR_LEN; }
    type decay() override { return type_; }
    [[nodiscard]] bool is_reference() const override { return true; }
    DEFAULT_VALUE_OVERRIDE("0")
  };
  struct lvalue_ref_t : public reference_t {
    using reference_t::reference_t;
    FORMAT_DECL_IMPL()
  };
  struct rvalue_ref_t : public reference_t {
    using reference_t::reference_t;
    FORMAT_DECL_IMPL()
  };
  struct pointer_t : public indirection_t {
    using indirection_t::indirection_t;
    [[nodiscard]] ushort size() const override { return MEM_ADDR_LEN; }
    bool operator==(const pointer_t& other) const;
    FORMAT_DECL_IMPL()
    DEFAULT_VALUE_OVERRIDE("0")
    [[nodiscard]] auto tie() const {
      return std::tie(const_, volatile_, type_);
    };
  };
  struct array_t : public compound_t {
    type items_t;
    unsigned short length;

    array_t(bool c, bool v, type t, short l)
        : compound_t(c, v),
          items_t(t),
          length(l) {}
    [[nodiscard]] ushort size() const override {
      return items_t->size() * length;
    }
    FORMAT_DECL_IMPL()
    [[nodiscard]] auto tie() const {
      return std::tie(const_, volatile_, items_t, length);
    };
  };
  struct function_t : public compound_t {
    std::vector<type> parameters_t;
    type return_t;

    // The size would be: one pointer per parameter, one pointer for the return
    // and another for the code.
    [[nodiscard]] ushort size() const override {
      return (parameters_t.size() + 2) * MEM_ADDR_LEN;
    }
    FORMAT_DECL_IMPL()
    [[nodiscard]] auto tie() const {
      return std::tie(const_, volatile_, parameters_t, return_t);
    };
  };
  struct enum_t : public compound_t {
    type underlying_t;
    enum_t();
    [[nodiscard]] ushort size() const override = 0;
    DEFAULT_VALUE_OVERRIDE("0")
    [[nodiscard]] auto tie() const {
      return std::tie(const_, volatile_, underlying_t);
    };
  };
  struct scoped_enum_t : public enum_t {
    [[nodiscard]] ushort size() const override { return underlying_t->size(); }
  };
  struct unscoped_enum_t : public enum_t {
    [[nodiscard]] ushort size() const override { return underlying_t->size(); }
  };
  struct class_t : public compound_t {
    [[nodiscard]] ushort size() const override { return 1; }
    [[nodiscard]] cstring default_value() const override { PANIC(); }
    [[nodiscard]] std::string format() const override { return ""; }
  };

  template <typename T>
  concept Type = std::is_base_of_v<base_type, std::remove_pointer_t<T>>;

  template <typename T>
  concept IndirectionType = std::is_base_of_v<indirection_t, T>;

  using qualtype          = const base_type*;

  struct store : default_singleton<store> {
    template <typename T, typename... Args>
    cv_type get_type(Args&&...);

  private:
    memory::Allocator m_allocator;
  };

  struct builder {
    STATIC_CLS(builder)
    template <typename T, typename... Args>
    cv_type get_type(bool const_, bool volatile_, Args&&...);

  private:
#ifdef TYPE_MAP_STORAGE
    struct store {
      template <Type T, typename... Args>
      static types::type get(bool const_, bool volatile_, Args&&...) noexcept;

    private:
      static store_t _store;

      template <Type T, typename... Args>
      static void add(bool const_, bool volatile_, Args&&...) noexcept;
      template <Type T, typename... Args>
      static bool contains(bool const_, bool volatile_, Args&&...) noexcept;
    };
#endif

  public:
    struct build_step {
      build_step& const_qual();
      build_step& volatile_qual();
      build_step& pointer_to();
      build_step& reference_of();
      template <typename T, typename... Args>
      types::type base_type(Args&&...);

    private:
      enum class step : uint8_t { const_ = 0, volatile_, pointer, reference };
      std::vector<step> steps;

      types::type parse_type(size_t&);
    };

    static build_step init();

    template <Type T>
    constexpr static type create(bool const_    = false,
                                 bool volatile_ = false) noexcept;
    template <typename... Args>
    constexpr static cv_type create(type_t,
                                    bool const_    = false,
                                    bool volatile_ = false,
                                    Args&&...);
  };

#define CV_STATIC_TYPE(CLS, NAME, CONST, VOLATILE) \
  constexpr const static inline auto CONCAT(_, NAME)  = CLS(); \
  constexpr const static inline auto CONCAT(NAME, _T) = &CONCAT(_, NAME);

#define STATIC_TYPE(CLS, NAME) \
  constexpr const static inline auto CONCAT(_, NAME)  = CLS(); \
  constexpr const static inline auto CONCAT(NAME, _T) = &CONCAT(_, NAME);

  STATIC_TYPE(void_t, VOID)
  STATIC_TYPE(uint_t, UINT)
  STATIC_TYPE(sint_t, INT)
  STATIC_TYPE(char_t, CHAR)
  STATIC_TYPE(float_t, FLOAT)
  const cv_type INTREF_T =
      builder::init().reference_of().base_type<types::sint_t>();
  const cv_type INTPTR_T =
      builder::init().pointer_to().base_type<types::sint_t>();

  template <typename T>
  struct is_type : std::is_same<T, type> {};

  static_assert(is_type<type>());

}; // namespace types

} // namespace cmm

static_assert(fmt::formattable<cmm::instruction_t>());
static_assert(std::formattable<cmm::instruction_t, char>);
#include "lang.inl"
