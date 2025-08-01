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

[[nodiscard]] constexpr const instruction_t::properties_map& instruction_t::properties_array() {
  using enum _instruction_t;
  static constexpr properties_map MAP{{{
      {0, false, {}},  // nop
      {1, false, {}},  // jmp
      {1, false, jne}, // je
      {1, false, je},  // jne
      {1, false, jnz}, // jz
      {1, false, jz},  // jnz
      {1, false, jle}, // jg
      {1, false, jl},  // jge
      {1, false, jge}, // jl
      {2, false, jg},  // jle
      {2, true, {}},   // mov
      {2, false, {}},  // lea
      {1, false, {}},  // push
      {1, false, {}},  // pop
      {2, true, {}},   // cmp
      {2, true, {}},   // test
      {2, true, {}},   // and
      {2, true, {}},   // or
      {2, true, {}},   // xor
      {1, true, {}},   // not
      {1, false, {}},  // inc
      {1, false, {}},  // dec
      {2, true, {}},   // add
      {2, true, {}},   // sub
      {2, true, {}},   // mul
      {2, true, {}},   // imul
      {2, true, {}},   // div
      {2, true, {}},   // idiv
      {0, false, {}},  // syscall
      {0, false, {}},  // ret
      {1, false, {}},  // call
      {1, false, {}},  // global
  }}};
  return MAP;
}

[[nodiscard]] constexpr const operator_t::properties_map& operator_t::properties_array() {
  static constexpr properties_map MAP{{{

      // Arithmetics
      {"+",
       6, // +
       associativity_t::L2R,
       {}},
      {"-",
       6, // -
       associativity_t::L2R,
       {}},
      {"*",
       5, // *
       associativity_t::L2R,
       {}},
      {"/",
       5, // /
       associativity_t::L2R,
       {}},

      // Inc / dec
      {"++",
       3, // preinc
       associativity_t::R2L,
       {}},
      {"--",
       3, // predec
       associativity_t::R2L,
       {}},
      {"++",
       2, // postinc
       associativity_t::L2R,
       {}},
      {"--",
       2, // postdec
       associativity_t::L2R,
       {}},

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
       instruction_t::jle},
      {"<=",
       9, // <=
       associativity_t::L2R,
       instruction_t::jl},
      {">",
       9, // >
       associativity_t::L2R,
       instruction_t::jge},
      {">=",
       9, // >=
       associativity_t::L2R,
       instruction_t::jg},

      // Logical
      {"^",
       12, // ^
       associativity_t::L2R,
       {}},
      {"||",
       15, // ||
       associativity_t::L2R,
       {}},
      {"&&",
       14, // &&
       associativity_t::L2R,
       {}},
      {"!",
       3, // !
       associativity_t::R2L,
       {}},
      {"&",
       3, // &
       associativity_t::R2L,
       {}},
      // Assignment
      {"=",
       16, // =
       associativity_t::R2L,
       {}}}}};

  return MAP;
}

}; // namespace cmm

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

// template <typename T, typename... Args>

// template <typename T, typename... Args>
// types::type builder::build_step::type_class(Args&&... args) {
//   bool c = false;
//   bool v = false;
//   if (steps[steps.size() - 1] == step::volatile_) {
//     v = true;
//     steps.pop_back();
//   }
//   if (steps[steps.size() - 1] == step::const_) {
//     c = true;
//     steps.pop_back();
//   }
//   auto* current = GET_TYPE(T, c, v, std::forward<Args>(args)...);
//   c             = false;
//   v             = false;
//   return std::ranges::fold_left(steps | std::views::reverse,
//                                 current,
//                                 [&c, &v](type curr, step next) -> type {
//                                   switch (next) {
//                                     case step::const_:
//                                       c = true;
//                                       break;
//                                     case step::volatile_:
//                                       v = true;
//                                       break;
//                                     case step::pointer:
//                                       {
//                                         const auto* ret =
//                                             GET_TYPE(pointer_t, c, v, curr);
//                                         c = false;
//                                         v = false;
//                                         return ret;
//                                       }
//                                     case step::reference:
//                                       {
//                                         const auto* ret =
//                                             GET_TYPE(lvalue_ref_t, c, v,
//                                             curr);
//                                         c = false;
//                                         v = false;
//                                         return ret;
//                                       }
//                                     default:
//                                       UNREACHABLE();
//                                   }
//                                   return curr;
//                                 });
// }
//
// template <Type T>
// constexpr type builder::create(bool const_, bool volatile_) noexcept {
//   return GET_TYPE(T, const_, volatile_);
// }
// template <typename... Args>
// constexpr cv_type builder::create(type_t t,
//                                   bool const_,
//                                   bool volatile_,
//                                   Args&&...) {
//   switch (t) {
//     case type_t::void_t:
//       return types::builder::create<types::void_t>();
//     case type_t::nullptr_t:
//       return types::builder::create<types::nullptr_t>();
//     case type_t::bool_t:
//       return types::builder::create<types::bool_t>(const_, volatile_);
//     case type_t::char_t:
//       return types::builder::create<types::char_t>(const_, volatile_);
//     case type_t::byte_t:
//       return types::builder::create<types::char_t>(const_, volatile_);
//     case type_t::int_t:
//       return types::builder::create<types::sint_t>(const_, volatile_);
//     case type_t::short_t:
//       return types::builder::create<types::sint_t>(const_, volatile_);
//     case type_t::double_t:
//       return types::builder::create<types::float_t>(const_, volatile_);
//     case type_t::long_t:
//       return types::builder::create<types::sint_t>(const_, volatile_);
//     case type_t::float_t:
//       return types::builder::create<types::float_t>(const_, volatile_);
//     case type_t::class_t:
//     case type_t::struct_t:
//     case type_t::enum_t:
//     default:
//       return types::builder::create<types::float_t>(const_, volatile_);
//       // return types::builder::create<types::class_t>(
//       //     const_, volatile_, std::forward<Args>(args)...);
//   }
// }
// } // namespace cmm
