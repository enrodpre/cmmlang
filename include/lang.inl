#pragma once

#include "lang.hpp"

#define TYPE_FORMAT_IMPL(TYPE, stdstr, ...) \
  constexpr std::string TYPE::format() const { \
    return std::format(stdstr, \
                       std::format("{}{}", \
                                   type_class::const_ ? "const " : "", \
                                   type_class::volatile_ ? "volatile " : ""), \
                       ##__VA_ARGS__); \
  }
#define CALL(op) op<T>(const_, volatile_, std::forward<Args>(args)...)
#define HASH()   std::hash<T>{}(const_, volatile_, std::forward<Args>(args)...)
#ifdef TYPE_MAP_STORAGE
  #define GET_TYPE(TYPE_, CONST, VOLATILE, ...) store::get<TYPE>(CONST, VOLATILE, __VA_ARGS__)
#else
  #define GET_TYPE(TYPE, CONST, VOLATILE, ...) \
    store::instance().get_type<TYPE>(CONST, VOLATILE, ##__VA_ARGS__)
#endif

namespace cmm {

[[nodiscard]] constexpr const builtin_signature_data::properties_map&
builtin_signature_data::properties_array() {
  using enum builtin_signature_t;
  static properties_map MAP{{{{MAIN, "main", {}},
                              {SYSCALL, "syscall", {}},
                              {EXIT, "exit", {UINT_T}},
                              {PRINT, "print", {UINT_T, CHAR_T}}}}};
  return MAP;
}

[[nodiscard]] constexpr const instruction_data::properties_map&
instruction_data::properties_array() {
  using enum instruction_t;
  using enum instruction_result_reg;
  static constexpr properties_map MAP{{{
      {nop, 0, false, {}},            // nop
      {jmp, 1, false, {}},            //
      {je, 1, false, {}},             //
      {jne, 1, false, {}},            //
      {jz, 1, false, {}},             //
      {jnz, 1, false, {}},            // jnz
      {jg, 1, false, {}},             //
      {jge, 1, false, {}},            // jge
      {jl, 1, false, {}},             //
      {jle, 2, false, {}},            // jle
      {mov, 2, true, {LEFT}},         //
      {lea, 2, false, {LEFT}},        // lea
      {push, 1, false, {}},           //
      {pop, 1, false, {}},            //
      {cmp, 2, true, {}},             //
      {test, 2, true, {}},            // test
      {and_, 2, true, {}},            //
      {or_, 2, true, {}},             // or
      {xor_, 2, true, {}},            //
      {not_, 1, true, {}},            // not
      {inc, 1, false, {LEFT}},        //
      {dec, 1, false, {LEFT}},        // dec
      {add, 2, true, {LEFT}},         //
      {sub, 2, true, {LEFT}},         // sub
      {mul, 1, true, {ACCUMULATOR}},  // mul
      {imul, 2, true, {ACCUMULATOR}}, // imul
      {div, 1, true, {ACCUMULATOR}},  // div
      {idiv, 2, true, {ACCUMULATOR}}, // idiv
      {syscall, 0, false, {}},        // syscall
      {ret, 0, false, {}},            // ret
      {call, 1, false, {}},           // call
      {global, 1, false, {}},         // global
      {address_of, 1, true, {}},      // address_of
      {deref, 1, true, {}},           // deref
  }}};
  return MAP;
}

[[nodiscard]] constexpr const operator_data::properties_map& operator_data::properties_array() {
  using enum operator_t;

  static constexpr properties_map MAP{{{

      // Arithmetics
      {plus,
       "+",
       6, // +
       associativity_t::L2R},
      {minus,
       "-",
       6, // -
       associativity_t::L2R},
      {star,
       "*",
       5, // *
       associativity_t::L2R},
      {fslash,
       "/",
       5, // /
       associativity_t::L2R},

      // Inc / dec
      {pre_inc,
       "++",
       3, // preinc
       associativity_t::R2L},
      {pre_dec,
       "--",
       3, // predec
       associativity_t::R2L},
      {post_inc,
       "++",
       2, // postinc
       associativity_t::L2R},
      {post_dec,
       "--",
       2, // postdec
       associativity_t::L2R},

      // Comparators
      {eq,
       "==",
       10, // ==
       associativity_t::L2R},

      {neq,
       "!=",
       10, // !=
       associativity_t::L2R},
      {le,
       "<",
       9, // <
       associativity_t::L2R},
      {lt,
       "<=",
       9, // <=
       associativity_t::L2R},
      {gt,
       ">",
       9, // >
       associativity_t::L2R},
      {gt,
       ">=",
       9, // >=
       associativity_t::L2R},

      // Logical
      {xor_,
       "^",
       12, // ^
       associativity_t::L2R},
      {or_,
       "||",
       15, // ||
       associativity_t::L2R},
      {and_,
       "&&",
       14, // &&
       associativity_t::L2R},
      {not_,
       "!",
       3, // !
       associativity_t::R2L},
      {ampersand,
       "&",
       3, // &
       associativity_t::R2L},
      // Assignment
      {assign,
       "=",
       16, // =
       associativity_t::R2L}}}};

  return MAP;
}

struct type_id {
  cmm::cstring name;
};

struct types_id {
  cmm::cstring data;
};

template <typename From, typename To>
struct converter {
  To operator()(const From&);
};

template <typename From, typename To>
struct type_converter {
  const converter<From, To>& from_to;
  const converter<To, From>& to_from;
};
}; // namespace cmm
