#pragma once

#include "common.hpp"
#include "types.hpp"
#include <cstddef>
#include <cstdint>
#include <fmt/base.h>
#include <frozen/unordered_map.h>
#include <magic_enum/magic_enum.hpp>
#include <magic_enum/magic_enum_format.hpp>

namespace cmm {

namespace ast::decl {
  class function;
}
using function_signature = std::pair<std::string, std::vector<ptr_type>>;
class mangled_name {

public:
  using value_type = std::string;
  explicit mangled_name(const value_type&);
  mangled_name(value_type&&);

  static mangled_name variable(cstring, cr_type);
  static mangled_name label(cstring);
  static mangled_name function(cstring, const std::vector<ptr_type>&);
  static mangled_name direct_conversion_function(cr_type, cr_type);
  static std::string types(const std::vector<const type*>&);

  [[nodiscard]] const value_type& str() const;
  operator std::string() const;

private:
  value_type m_string;
};

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

  // Not instructions
  address_of,
  deref,
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

enum class _builtin_signature_t : uint8_t { MAIN, SYSCALL, EXIT, PRINT };
using header_arguments_t = std::vector<ptr_type>;
struct builtin_signature_t : public cmm::enumeration<_builtin_signature_t> {
  BUILD_ENUMERATION(builtin_signature_t, std::string_view, function_name, header_arguments_t, args);

  [[nodiscard]] function_signature signature() const { return {std::string(function_name), args}; }
  [[nodiscard]] std::string mangle() const { return mangled_name::function(function_name, args); }
};
enum class keyword_t : uint8_t { IF, WHILE, FOR, GOTO, BREAK, CONTINUE, RETURN };

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

enum class attribute : uint8_t {
  no_return          = 1 << 1,
  carries_dependency = 1 << 2,
  deprecated         = 1 << 3,
};

enum class linkage_t : uint8_t { normal = 0, internal, external };
enum class storage_t : uint8_t { normal = 0, static_, extern_, mutable_, register_ };

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

template <typename T>
struct storage {
  virtual T& stored() const = 0;
};
template <typename T>
struct static_storage : public storage<T> {};
template <typename T>
struct dynamic_storage : public storage<T> {
  uintptr_t address;
};

#define DEFINE_STATIC_SIZE(TYPE, VALUE) \
  template <> struct sizeof_<TYPE> { \
    static constexpr size_t value = VALUE; \
  };

struct value {};
struct scalar_value : public value {
  // type_t type;
};
struct composite_value : public value {
  size_t length;
};
struct object;

// struct typeof {
//   STATIC_CLS(typeof);
//   constexpr static category_t operator()(const type&);
// };

struct sizeof_ {
  STATIC_CLS(sizeof_);
  constexpr static size_t operator()(const type&);
  constexpr static size_t operator()(const object&);
};

struct align {};

struct object {
  std::string name;
  align alignment;
  storage_t storage;
  cmm::type type;
  cmm::value* value;
};
} // namespace cmm
static_assert(std::formattable<cmm::instruction_t, char>);

#include "lang.inl"
