#pragma once

#include "lang.hpp"

namespace cmm {
enum class associativity_t : uint8_t;
} // namespace cmm

#define TYPE_FORMAT_IMPL(TYPE, stdstr, ...)                                   \
  constexpr std::string TYPE::format() const {                                \
    return std::format(stdstr,                                                \
                       std::format("{}{}",                                    \
                                   type_class::const_ ? "const " : "",        \
                                   type_class::volatile_ ? "volatile " : ""), \
                       ##__VA_ARGS__);                                        \
  }
#define CALL(op) op<T>(const_, volatile_, std::forward<Args>(args)...)
#define HASH()   std::hash<T>{}(const_, volatile_, std::forward<Args>(args)...)
#ifdef TYPE_MAP_STORAGE
#  define GET_TYPE(TYPE_, CONST, VOLATILE, ...) store::get<TYPE>(CONST, VOLATILE, __VA_ARGS__)
#else
#  define GET_TYPE(TYPE, CONST, VOLATILE, ...)                       \
    store::instance().get_type<TYPE>(CONST, VOLATILE, ##__VA_ARGS__)
#endif

namespace cmm {

[[nodiscard]] constexpr const builtin_signature_data::properties_map&
builtin_signature_data::properties_array() {
  using enum builtin_signature_t;
  static properties_map MAP{{{
      {MAIN, "main", {}}, {SYSCALL, "syscall", {}},
      // {EXIT, "exit", {types::arena().make(types::category_t::uint_t, {})}},
      // {PRINT, "print", {UINT_T, CHAR_T}}
  }}};
  return MAP;
}

} // namespace cmm
