#pragma once

#include "common.hpp"
#include "types.hpp"
#include <cstddef>
#include <cstdint>
#include <frozen/unordered_map.h>
#include <magic_enum/magic_enum.hpp>
#include <magic_enum/magic_enum_containers.hpp>
#include <magic_enum/magic_enum_format.hpp>

namespace cmm {

namespace ast::decl {
  class function;
  class signature;
} // namespace ast::decl
class mangled_name {

public:
  using value_type = std::string;
  mangled_name(const value_type& v)
      : m_string(v) {}
  mangled_name(value_type&& v)
      : m_string(std::move(v)) {}

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

enum class instruction_result_reg : uint8_t { NONE, LEFT, RIGHT, ACCUMULATOR };
enum class instruction_t : uint8_t {
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

enum class operator_t : uint8_t {
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
  o_paren,
  c_paren,
  o_bracket,
  c_bracket,
  o_curly,
  c_curly
};

enum class associativity_t : uint8_t { Either, L2R, R2L };
BUILD_ENUMERATION_DATA_CLASS(operator,
                             std::string,
                             repr,
                             uint8_t,
                             precedence,
                             associativity_t,
                             assoc)

BUILD_ENUMERATION_DATA_CLASS(instruction,
                             short,
                             n_params,
                             bool,
                             can_address_memory,
                             instruction_result_reg,
                             where);

enum class builtin_signature_t : uint8_t { MAIN, SYSCALL, EXIT, PRINT };
using header_arguments_t = std::vector<ptr_type>;
struct builtin_signature_data : public cmm::enumeration<builtin_signature_t>, public displayable {
  BUILD_ENUMERATION_DATA(builtin_signature,
                         std::string_view,
                         function_name,
                         header_arguments_t,
                         args);
  [[nodiscard]] ast::decl::signature signature() const;
};
enum class keyword_t : uint8_t { IF, WHILE, FOR, GOTO, BREAK, CONTINUE, RETURN };

enum class arg_t : uint8_t { NONE, LEFT, RIGHT, ACC, AUX1, AUX2 };

using execution = std::pair<instruction_t, std::array<arg_t, 2>>;

#define CREATE_SIMPLE_INS(INS) \
  {std::make_pair(instruction_t ::INS, std::make_pair(arg_t::LEFT, arg_t::RIGHT))}

#define CREATE_COND(INS) \
  { \
    std::make_pair(instruction_t ::cmp, std::make_pair(arg_t::LEFT, arg_t::RIGHT)), { \
      instruction_t ::INS, {} \
    } \
  }

#define CREATE_CONDITION(OP, INS) \
  { \
      {{instruction_t ::cmp, {arg_t::LEFT, arg_t::RIGHT}}, {instruction_t ::INS, {}}} \
},

constexpr magic_enum::containers::
    array<operator_t, std::array<std::pair<instruction_t, std::pair<arg_t, arg_t>>, 3>>
        builtin_operators{
            {{CREATE_SIMPLE_INS(add),
              CREATE_SIMPLE_INS(sub),
              {std::make_pair(instruction_t::mov, std::make_pair(arg_t::ACC, arg_t::LEFT)),
               std::make_pair(instruction_t ::mul, std::make_pair(arg_t ::RIGHT, arg_t::NONE)),
               std::make_pair(instruction_t ::mov, std::make_pair(arg_t ::LEFT, arg_t::ACC))},
              {std::make_pair(instruction_t::mov, std::make_pair(arg_t::ACC, arg_t::LEFT)),
               std::make_pair(instruction_t ::div, std::make_pair(arg_t ::RIGHT, arg_t::NONE)),
               std::make_pair(instruction_t ::mov, std::make_pair(arg_t ::LEFT, arg_t::ACC))},
              CREATE_SIMPLE_INS(inc),
              CREATE_SIMPLE_INS(dec),
              CREATE_SIMPLE_INS(inc),
              CREATE_SIMPLE_INS(dec),
              CREATE_COND(jne),
              CREATE_COND(je),
              CREATE_COND(jge),
              CREATE_COND(jg),
              CREATE_COND(jle),
              CREATE_COND(jl),
              CREATE_SIMPLE_INS(xor_),
              CREATE_SIMPLE_INS(or_),
              CREATE_SIMPLE_INS(and_),
              CREATE_SIMPLE_INS(not_),
              {},
              CREATE_SIMPLE_INS(mov)}}};

constexpr auto get_builtin_operator(operator_t op) {
  return builtin_operators.at(op) |
         std::views::filter([](const auto& ins) { return ins.first != instruction_t::nop; });
}
// constexpr auto get_builtin_operator(operator_t)
static_assert(magic_enum::enum_count<operator_t>() == builtin_operators.size());

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
