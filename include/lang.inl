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

[[nodiscard]] constexpr const builtin_signature_t::properties_map&
builtin_signature_t::properties_array() {
  static properties_map MAP{
      {{{"main", {}}, {"syscall", {}}, {"exit", {UINT_T}}, {"print", {UINT_T, CHAR_T}}}}};
  return MAP;
}

[[nodiscard]] constexpr const instruction_t::properties_map& instruction_t::properties_array() {
  using enum _instruction_t;
  static constexpr properties_map MAP{{{
      {0, false, {}},    // nop
      {1, false, {nop}}, // jmp
      {1, false, jne},   // je
      {1, false, je},    // jne
      {1, false, jnz},   // jz
      {1, false, jz},    // jnz
      {1, false, jle},   // jg
      {1, false, jl},    // jge
      {1, false, jge},   // jl
      {2, false, jg},    // jle
      {2, true, {}},     // mov
      {2, false, {}},    // lea
      {1, false, {}},    // push
      {1, false, {}},    // pop
      {2, true, {}},     // cmp
      {2, true, {}},     // test
      {2, true, {}},     // and
      {2, true, {}},     // or
      {2, true, {}},     // xor
      {1, true, {}},     // not
      {1, false, {}},    // inc
      {1, false, {}},    // dec
      {2, true, {}},     // add
      {2, true, {}},     // sub
      {1, true, {}},     // mul
      {2, true, {}},     // imul
      {1, true, {}},     // div
      {2, true, {}},     // idiv
      {0, false, {}},    // syscall
      {0, false, {}},    // ret
      {1, false, {}},    // call
      {1, false, {}},    // global
      {1, true, {}},     // address_of
      {1, true, {}},     // deref
  }}};
  return MAP;
}

[[nodiscard]] constexpr const operator_t::properties_map& operator_t::properties_array() {
  using enum _instruction_t;
  static constexpr properties_map MAP{{{

      // Arithmetics
      {"+",
       6, // +
       associativity_t::L2R,
       {add}},
      {"-",
       6, // -
       associativity_t::L2R,
       {sub}},
      {"*",
       5, // *
       associativity_t::L2R,
       {mul}},
      {"/",
       5, // /
       associativity_t::L2R,
       {}},

      // Inc / dec
      {"++",
       3, // preinc
       associativity_t::R2L,
       {inc}},
      {"--",
       3, // predec
       associativity_t::R2L,
       {dec}},
      {"++",
       2, // postinc
       associativity_t::L2R,
       {inc}},
      {"--",
       2, // postdec
       associativity_t::L2R,
       {dec}},

      // Comparators
      {"==",
       10, // ==
       associativity_t::L2R,
       instruction_t::je},

      {"!=",
       10, // !=
       associativity_t::L2R,
       instruction_t::jne},
      {"<",
       9, // <
       associativity_t::L2R,
       instruction_t::jl},
      {"<=",
       9, // <=
       associativity_t::L2R,
       instruction_t::jle},
      {">",
       9, // >
       associativity_t::L2R,
       instruction_t::jg},
      {">=",
       9, // >=
       associativity_t::L2R,
       instruction_t::jge},

      // Logical
      {"^",
       12, // ^
       associativity_t::L2R,
       {xor_}},
      {"||",
       15, // ||
       associativity_t::L2R,
       {or_}},
      {"&&",
       14, // &&
       associativity_t::L2R,
       {and_}},
      {"!",
       3, // !
       associativity_t::R2L,
       {not_}},
      {"&",
       3, // &
       associativity_t::R2L,
       {address_of}},
      // Assignment
      {"=",
       16, // =
       associativity_t::R2L,
       {deref}}}}};

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
