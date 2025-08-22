/// @brief Some useful common macros.
/// @link  https://nessan.github.io/utilities/
/// NOTE:  MSVC's traditional preprocessor barfs on these macros but their newer
/// cross platform compatible one is fine.
///        To use the upgrade, add the '/Zc:preprocessor' flag at compile time.
///        Our CMake module `compiler_init` does that for you.
/// SPDX-FileCopyrightText:  2024 Nessan Fitzmaurice <nessan.fitzmaurice@me.com>
/// SPDX-License-Identifier: MIT
#pragma once

#include <print>
/// @brief Invoke the pre-processor stringizing operator but fully expanding any
/// macro argument first!
#define STRINGIZE(s)      STRINGIZE_IMPL(s)
#define STRINGIZE_IMPL(s) #s

/// @brief Concatenate two symbols but making sure to fully expand those symbols
/// if they happen to be macros themselves.
#define CONCAT(a, b)      CONCAT_IMPL(a, b)
#define CONCAT_IMPL(a, b) a##b

/// @brief Turn a semantic version into a string (we overload to handle 1, 2, or
/// 3 argument versions)
#define VERSION_STRING(...) OVERLOAD(VERSION_STRING, __VA_ARGS__)

// The actual one, two, and three argument versions of that macro
// NOTE: In C++ contiguous strings are concatenated so "2" "." "3" is the same
// as "2.3"
#define VERSION_STRING1(major)        STRINGIZE(major)
#define VERSION_STRING2(major, minor) STRINGIZE(major) "." STRINGIZE(minor)
#define VERSION_STRING3(major, minor, patch) \
  STRINGIZE(major) "." STRINGIZE(minor) "." STRINGIZE(patch)

/// @brief RUN(code); prints the line of code to the console and then executes
/// it.
/// @note This is a an overloaded macro that is used in some test/example codes
/// to show what specific code is getting executed, optionally followed by one
/// or two results from that call.
#define RUN(...) OVERLOAD(RUN, __VA_ARGS__)

// The one, two,and three argument versions of RUN
#define RUN1(code) \
  std::cout << "[CODE]   " << #code << "\n"; \
  code

#define RUN2(code, val) \
  RUN1(code); \
  std::cout << "[RESULT] " << #val << ": " << val << '\n'

#define RUN3(code, val1, val2) \
  RUN1(code); \
  std::cout << "[RESULT] " << #val1 << ": " << val1 << " and " << #val2 << ": " << val2 << '\n'

/// @brief   Preprocessor trickery to allow for macros that can be overloaded by
/// the number of passed arguments.
/// @example #define FOO(...) OVERLOAD(FOO, __VA_ARGS__) will make 'FOO'
/// overloaded on the number of passed args.
////         So FOO() will call the zero arg version FOO0(), FOO(a) will call
/// FOO1(a), FOO(a,b) will call FOO2(a,b) etc.
/// @note    You supply whichever specific FOO0(), FOO1(a), FOO2(a,b),
/// FOO2(a,b,c), implementations that make sense,
///          but the consumer can just call FOO(...) and automatically get the
///          correct one.
#define OVERLOAD(macro, ...) CONCAT(macro, ARG_COUNT(__VA_ARGS__))(__VA_ARGS__)

/// @brief ARG_COUNT(...) expands to the count of its arguments e.g.
/// ARG_COUNT(x,y,z) will expands to 3.
#define ARG_COUNT(...)                                                 ARG_COUNT_IMPL(__VA_ARGS__ __VA_OPT__(, ) 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#define ARG_COUNT_IMPL(_1, _2, _3, _4, _5, _6, _7, _8, _9, count, ...) count

/// @brief Compiler name & version as a string -- occasionally useful to have
/// around to annotate test output etc.
/// @note Could add more compilers from e.g.
/// https://github.com/cpredef/predef/blob/master/Compilers.md)
#if defined(__clang__)
  #define COMPILER_NAME \
    "clang " VERSION_STRING(__clang_major__, __clang_minor__, __clang_patchlevel__)
#elif defined(__GNUC__)
  #define COMPILER_NAME "gcc " VERSION_STRING(__GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__)
#else
  #define COMPILER_NAME "Unidentified Compiler"
#endif

#define MOVABLE_CLS(CLS) \
  CLS(CLS&&) noexcept            = default; \
  CLS& operator=(CLS&&) noexcept = default;

#define COPYABLE_CLS(CLS) \
  CLS(const CLS&)            = default; \
  CLS& operator=(const CLS&) = default;

#define DEFAULT_CLASS(CLS) \
  CLS()  = default; \
  ~CLS() = default; \
  COPYABLE_CLS(CLS) \
  MOVABLE_CLS(CLS)

#define NOT_MOVABLE_CLS(CLS) \
  CLS(CLS&&)            = delete; \
  CLS& operator=(CLS&&) = delete;

#define NOT_COPYABLE_CLS(CLS) \
  CLS(const CLS&)            = delete; \
  CLS& operator=(const CLS&) = delete;

#define STATIC_CLS(CLS) \
  CLS()  = delete; \
  ~CLS() = delete; \
  NOT_COPYABLE_CLS(CLS) \
  NOT_MOVABLE_CLS(CLS)

#define AST_SIBLINGS(...) \
  std::string string() const override { return cpptrace::demangle(typeid(this).name()); }
// std::vector<node*> children() const override { return
// transform(CAST_TO_NODE); }
#define AST_COMPOSITE(...) \
  std::string string() const override { return cpptrace::demangle(typeid(this).name()); }
// std::vector<node*> children() const override { \
  //   return std::vector<node*>{__VA_ARGS__} | \
  //          std::views::transform([](auto&& elem) { return dynamic_cast<node*>(elem); }); \
  // }
#define AST_LEAF \
  std::string string() const override { return cpptrace::demangle(typeid(this).name()); }
// std::vector<node*> children() const override { return {}; }

#define ENUM_PROPERTY(TYPE, NAME, N) TYPE NAME

// Helper macro to extract types (every odd-positioned argument: 1st, 3rd, 5th,
// etc.)
#define GET_TYPES_1(t1, ...)                                             t1
#define GET_TYPES_2(t1, n1, t2, ...)                                     t1, t2
#define GET_TYPES_3(t1, n1, t2, n2, t3, ...)                             t1, t2, t3
#define GET_TYPES_4(t1, n1, t2, n2, t3, n3, t4, ...)                     t1, t2, t3, t4
#define GET_TYPES_5(t1, n1, t2, n2, t3, n3, t4, n4, t5, ...)             t1, t2, t3, t4, t5
#define GET_TYPES_6(t1, n1, t2, n2, t3, n3, t4, n4, t5, n5, t6, ...)     t1, t2, t3, t4, t5, t6
#define GET_TYPES_7(t1, n1, t2, n2, t3, n3, t4, n4, t5, n5, t6, t7, ...) t1, t2, t3, t4, t5, t6, t7
#define GET_TYPES_8(t1, n1, t2, n2, t3, n3, t4, n4, t5, n5, t6, t7, t8...) \
  t1, t2, t3, t4, t5, t6, t7, t8
#define GET_TYPES_9(t1, n1, t2, n2, t3, n3, t4, n4, t5, n5, t6, t7, t8, t9...) \
  t1, t2, t3, t4, t5, t6, t7, t8, t9
#define GET_TYPES_10(t1, n1, t2, n2, t3, n3, t4, n4, t5, n5, t6, t7, t8, t9, t10...) \
  t1, t2, t3, t4, t5, t6, t7, t8, t9, t10
#define GET_TYPES_12(t1, n1, t2, n2, t3, n3, t4, n4, t5, n5, t6, t7, t8, t9, t10, t11, t12...) \
  t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12

// Helper macro to declare variables
#define DECLARE_VARS_2(t1, n1) ENUM_PROPERTY(t1, n1, 0);

#define DECLARE_VARS_4(t1, n1, t2, n2) \
  ENUM_PROPERTY(t1, n1, 0); \
  ENUM_PROPERTY(t2, n2, 1);
#define DECLARE_VARS_6(t1, n1, t2, n2, t3, n3) \
  ENUM_PROPERTY(t1, n1, 0); \
  ENUM_PROPERTY(t2, n2, 1); \
  ENUM_PROPERTY(t3, n3, 2);
#define DECLARE_VARS_8(t1, n1, t2, n2, t3, n3, t4, n4) \
  ENUM_PROPERTY(t1, n1, 0); \
  ENUM_PROPERTY(t2, n2, 1); \
  ENUM_PROPERTY(t3, n3, 2); \
  ENUM_PROPERTY(t4, n4, 3);
#define DECLARE_VARS_10(t1, n1, t2, n2, t3, n3, t4, n4, t5, n5) \
  ENUM_PROPERTY(t1, n1, 0); \
  ENUM_PROPERTY(t2, n2, 1); \
  ENUM_PROPERTY(t3, n3, 2); \
  ENUM_PROPERTY(t4, n4, 3); \
  ENUM_PROPERTY(t5, n5, 4);
#define DECLARE_VARS_12(t1, n1, t2, n2, t3, n3, t4, n4, t5, n5, t6, n6) \
  ENUM_PROPERTY(t1, n1, 0); \
  ENUM_PROPERTY(t2, n2, 1); \
  ENUM_PROPERTY(t3, n3, 2); \
  ENUM_PROPERTY(t4, n4, 3); \
  ENUM_PROPERTY(t5, n5, 4); \
  ENUM_PROPERTY(t6, n6, 5);

#define CTOR_PARAMS_2(t1, n1)                         t1 _##n1
#define CTOR_PARAMS_4(t1, n1, t2, n2)                 t1 _##n1, t2 _##n2
#define CTOR_PARAMS_6(t1, n1, t2, n2, t3, n3)         t1 _##n1, t2 _##n2, t3 _##n3
#define CTOR_PARAMS_8(t1, n1, t2, n2, t3, n3, t4, n4) t1 _##n1, t2 _##n2, t3 _##n3, t4 _##n4
#define CTOR_PARAMS_10(t1, n1, t2, n2, t3, n3, t4, n4, t5, n5) \
  t1 _##n1, t2 _##n2, t3 _##n3, t4 _##n4, t5 _##n5
#define CTOR_PARAMS_12(t1, n1, t2, n2, t3, n3, t4, n4, t5, n5, t6, n6) \
  t1 _##n1, t2 _##n2, t3 _##n3, t4 _##n4, t5 _##n5, t6 _##n6

#define GET_DATA_VALUE(N)                  std::get<N + 1>(element_type::properties_array().at(e))
#define CTOR_ASSIGN_DATA_2(t1, n1)         n1(GET_DATA_VALUE(0))
#define CTOR_ASSIGN_DATA_4(t1, n1, t2, n2) n1(GET_DATA_VALUE(0)), n2(GET_DATA_VALUE(1))
#define CTOR_ASSIGN_DATA_6(t1, n1, t2, n2, t3, n3) \
  n1(GET_DATA_VALUE(0)), n2(GET_DATA_VALUE(1)), n3(GET_DATA_VALUE(2))
#define CTOR_ASSIGN_DATA_8(t1, n1, t2, n2, t3, n3, t4, n4) \
  n1(GET_DATA_VALUE(0)), n2(GET_DATA_VALUE(1)), n3(GET_DATA_VALUE(2)), n4(GET_DATA_VALUE(3))
#define CTOR_ASSIGN_DATA_10(t1, n1, t2, n2, t3, n3, t4, n4, t5, n5) \
  n1(GET_DATA_VALUE(0)), n2(GET_DATA_VALUE(1)), n3(GET_DATA_VALUE(2)), n4(GET_DATA_VALUE(3)), \
      n5(GET_DATA_VALUE(4))
#define CTOR_ASSIGN_DATA_12(t1, n1, t2, n2, t3, n3, t4, n4, t5, n5, t6, n6) \
  n1(GET_DATA_VALUE(0)), n2(GET_DATA_VALUE(1)), n3(GET_DATA_VALUE(2)), n4(GET_DATA_VALUE(3)), \
      n5(GET_DATA_VALUE(4)), n6(GET_DATA_VALUE(5))

// Count arguments
#define GET_ARG_COUNT(...) \
  GET_ARG_COUNT_IMPL( \
      __VA_ARGS__, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
#define GET_ARG_COUNT_IMPL(_1, \
                           _2, \
                           _3, \
                           _4, \
                           _5, \
                           _6, \
                           _7, \
                           _8, \
                           _9, \
                           _10, \
                           _11, \
                           _12, \
                           _13, \
                           _14, \
                           _15, \
                           _16, \
                           _17, \
                           _18, \
                           _19, \
                           _20, \
                           N, \
                           ...) \
  N

#define CONSTRUCT_VARS_2(t1, n1) , n1()

// Dispatch macros
#define DECLARE_VARS(...)     CONCAT(DECLARE_VARS_, GET_ARG_COUNT(__VA_ARGS__))(__VA_ARGS__)
#define GET_TYPES(...)        CONCAT(GET_TYPES_, GET_PAIR_COUNT(__VA_ARGS__))(__VA_ARGS__)
#define CTOR_PARAMS(...)      CONCAT(CTOR_PARAMS_, GET_ARG_COUNT(__VA_ARGS__))(__VA_ARGS__)
#define CTOR_ASSIGN_DATA(...) CONCAT(CTOR_ASSIGN_DATA_, GET_ARG_COUNT(__VA_ARGS__))(__VA_ARGS__)

// Calculate number of pairs (divide arg count by 2)
#define GET_PAIR_COUNT(...)    GET_PAIR_COUNT_IMPL(GET_ARG_COUNT(__VA_ARGS__))
#define GET_PAIR_COUNT_IMPL(n) CONCAT(PAIR_COUNT_, n)
#define PAIR_COUNT_2           1
#define PAIR_COUNT_4           2
#define PAIR_COUNT_6           3
#define PAIR_COUNT_8           4
#define PAIR_COUNT_10          5
#define PAIR_COUNT_12          6
#define PAIR_COUNT_14          7
#define PAIR_COUNT_16          8
#define PAIR_COUNT_18          9
#define PAIR_COUNT_20          10
#define PAIR_COUNT_22          11

#define BUILD_ENUMERATION_DATA(TYPE, ...) \
  using value_type   = CONCAT(TYPE, _t); \
  using element_type = CONCAT(TYPE, _data); \
  using enum value_type; \
  const value_type self; \
  DECLARE_VARS(__VA_ARGS__) \
  using member_types   = std::tuple<value_type, GET_TYPES(__VA_ARGS__)>; \
  using properties_map = magic_enum::containers::array<value_type, member_types>; \
  [[nodiscard]] std::string string() const override { return std::format("{}", self); } \
  static_assert(std::is_constant_evaluated()); \
  [[nodiscard]] static constexpr const properties_map& properties_array(); \
  constexpr CONCAT(TYPE, _data)(value_type e) \
      : self(e), \
        CTOR_ASSIGN_DATA(__VA_ARGS__) {}

#define BUILD_ENUMERATION_DATA_CLASS(TYPE, ...) \
  struct CONCAT(TYPE, _data) \
      : public displayable { \
    BUILD_ENUMERATION_DATA(TYPE, __VA_ARGS__) \
  };
