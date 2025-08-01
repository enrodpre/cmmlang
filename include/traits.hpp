#pragma once

#include <concepts>
#include <functional>
#include <memory>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>

// Helper macros for variadic argument handling
#define FIRST(a, ...)     a
#define SECOND(a, b, ...) b
#define EMPTY()
#define EVAL(...)     EVAL1024(__VA_ARGS__)
#define EVAL1024(...) EVAL512(EVAL512(__VA_ARGS__))
#define EVAL512(...)  EVAL256(EVAL256(__VA_ARGS__))
#define EVAL256(...)  EVAL128(EVAL128(__VA_ARGS__))
#define EVAL128(...)  EVAL64(EVAL64(__VA_ARGS__))
#define EVAL64(...)   EVAL32(EVAL32(__VA_ARGS__))
#define EVAL32(...)   EVAL16(EVAL16(__VA_ARGS__))
#define EVAL16(...)   EVAL8(EVAL8(__VA_ARGS__))
#define EVAL8(...)    EVAL4(EVAL4(__VA_ARGS__))
#define EVAL4(...)    EVAL2(EVAL2(__VA_ARGS__))
#define EVAL2(...)    EVAL1(EVAL1(__VA_ARGS__))
#define EVAL1(...)    __VA_ARGS__

// Check if argument list is empty
#define IS_EMPTY(...)          IS_EMPTY_HELPER(FIRST(__VA_ARGS__, ))
#define IS_EMPTY_HELPER(x)     IS_EMPTY_HELPER2(x, 1)
#define IS_EMPTY_HELPER2(x, n) IS_EMPTY_HELPER3(IS_EMPTY_HELPER_##x, n)
#define IS_EMPTY_HELPER3(a, n) IS_EMPTY_HELPER4(a, n)
#define IS_EMPTY_HELPER4(a, n) IS_EMPTY_HELPER5(a, n)
#define IS_EMPTY_HELPER5(a, n) IS_EMPTY_HELPER6(a, n)
#define IS_EMPTY_HELPER6(a, n) IS_EMPTY_HELPER7(a, n)
#define IS_EMPTY_HELPER7(a, n) a n
#define IS_EMPTY_HELPER_       0,

// Check if there are more arguments
#define HAS_MORE(...) \
  HAS_MORE_HELPER(__VA_ARGS__, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  1, \
                  0)
#define HAS_MORE_HELPER(a1, \
                        a2, \
                        a3, \
                        a4, \
                        a5, \
                        a6, \
                        a7, \
                        a8, \
                        a9, \
                        a10, \
                        a11, \
                        a12, \
                        a13, \
                        a14, \
                        a15, \
                        a16, \
                        a17, \
                        a18, \
                        a19, \
                        a20, \
                        a21, \
                        a22, \
                        a23, \
                        a24, \
                        a25, \
                        a26, \
                        a27, \
                        a28, \
                        a29, \
                        a30, \
                        a31, \
                        a32, \
                        a33, \
                        a34, \
                        a35, \
                        a36, \
                        a37, \
                        a38, \
                        a39, \
                        a40, \
                        a41, \
                        a42, \
                        a43, \
                        a44, \
                        a45, \
                        a46, \
                        a47, \
                        a48, \
                        a49, \
                        a50, \
                        a51, \
                        a52, \
                        a53, \
                        a54, \
                        a55, \
                        a56, \
                        a57, \
                        a58, \
                        a59, \
                        a60, \
                        a61, \
                        a62, \
                        a63, \
                        N, \
                        ...) \
  N

// Conditional macro
#define IF(condition, then, else)        IF_HELPER(condition, then, else)
#define IF_HELPER(condition, then, else) IF_##condition(then, else)
#define IF_0(then, else)                 else
#define IF_1(then, else)                 then

// Defer macro for recursion
#define DEFER(macro) macro EMPTY()

// --- Reference manipulation ---
#define ADD_REF_TO_SINGLE(type) type&
#define ADD_REF_IMPL(type, ...) \
  ADD_REF_TO_SINGLE(type) \
  IF(HAS_MORE(__VA_ARGS__), DEFER(ADD_REF_COMMA)(__VA_ARGS__), )
#define ADD_REF_COMMA(...)  , DEFER(ADD_REF_IMPL)(__VA_ARGS__)
#define ADD_REFERENCES(...) EVAL(ADD_REF_IMPL(__VA_ARGS__))

// --- Const manipulation ---
#define ADD_CONST_TO_SINGLE(type) const type
#define ADD_CONST_IMPL(type, ...) \
  ADD_CONST_TO_SINGLE(type) \
  IF(HAS_MORE(__VA_ARGS__), DEFER(ADD_CONST_COMMA)(__VA_ARGS__), )
#define ADD_CONST_COMMA(...) , DEFER(ADD_CONST_IMPL)(__VA_ARGS__)
#define ADD_CONST(...)       EVAL(ADD_CONST_IMPL(__VA_ARGS__))

// --- Pointer manipulation ---
#define ADD_PTR_TO_SINGLE(type) type*
#define ADD_PTR_IMPL(type, ...) \
  ADD_PTR_TO_SINGLE(type) \
  IF(HAS_MORE(__VA_ARGS__), DEFER(ADD_PTR_COMMA)(__VA_ARGS__), )
#define ADD_PTR_COMMA(...) , DEFER(ADD_PTR_IMPL)(__VA_ARGS__)
#define ADD_POINTERS(...)  EVAL(ADD_PTR_IMPL(__VA_ARGS__))

// --- Const reference manipulation ---
#define ADD_CONST_REF_TO_SINGLE(type) const type&
#define ADD_CONST_REF_IMPL(type, ...) \
  ADD_CONST_REF_TO_SINGLE(type) \
  IF(HAS_MORE(__VA_ARGS__), DEFER(ADD_CONST_REF_COMMA)(__VA_ARGS__), )
#define ADD_CONST_REF_COMMA(...)  , DEFER(ADD_CONST_REF_IMPL)(__VA_ARGS__)
#define ADD_CONST_REFERENCES(...) EVAL(ADD_CONST_REF_IMPL(__VA_ARGS__))

// --- Rvalue reference manipulation ---
#define ADD_RREF_TO_SINGLE(type) type&&
#define ADD_RREF_IMPL(type, ...) \
  ADD_RREF_TO_SINGLE(type) \
  IF(HAS_MORE(__VA_ARGS__), DEFER(ADD_RREF_COMMA)(__VA_ARGS__), )
#define ADD_RREF_COMMA(...)        , DEFER(ADD_RREF_IMPL)(__VA_ARGS__)
#define ADD_RVALUE_REFERENCES(...) EVAL(ADD_RREF_IMPL(__VA_ARGS__))

// --- Const pointer manipulation ---
#define ADD_CONST_PTR_TO_SINGLE(type) const type*
#define ADD_CONST_PTR_IMPL(type, ...) \
  ADD_CONST_PTR_TO_SINGLE(type) \
  IF(HAS_MORE(__VA_ARGS__), DEFER(ADD_CONST_PTR_COMMA)(__VA_ARGS__), )
#define ADD_CONST_PTR_COMMA(...) , DEFER(ADD_CONST_PTR_IMPL)(__VA_ARGS__)
#define ADD_CONST_POINTERS(...)  EVAL(ADD_CONST_PTR_IMPL(__VA_ARGS__))

// --- Volatile manipulation ---
#define ADD_VOLATILE_TO_SINGLE(type) volatile type
#define ADD_VOLATILE_IMPL(type, ...) \
  ADD_VOLATILE_TO_SINGLE(type) \
  IF(HAS_MORE(__VA_ARGS__), DEFER(ADD_VOLATILE_COMMA)(__VA_ARGS__), )
#define ADD_VOLATILE_COMMA(...) , DEFER(ADD_VOLATILE_IMPL)(__VA_ARGS__)
#define ADD_VOLATILE(...)       EVAL(ADD_VOLATILE_IMPL(__VA_ARGS__))

// --- Const volatile manipulation ---
#define ADD_CONST_VOLATILE_TO_SINGLE(type) const volatile type
#define ADD_CONST_VOLATILE_IMPL(type, ...) \
  ADD_CONST_VOLATILE_TO_SINGLE(type) \
  IF(HAS_MORE(__VA_ARGS__), DEFER(ADD_CONST_VOLATILE_COMMA)(__VA_ARGS__), )
#define ADD_CONST_VOLATILE_COMMA(...) , DEFER(ADD_CONST_VOLATILE_IMPL)(__VA_ARGS__)
#define ADD_CONST_VOLATILE(...)       EVAL(ADD_CONST_VOLATILE_IMPL(__VA_ARGS__))

// --- Pointer to pointer manipulation ---
#define ADD_PTR_PTR_TO_SINGLE(type) type**
#define ADD_PTR_PTR_IMPL(type, ...) \
  ADD_PTR_PTR_TO_SINGLE(type) \
  IF(HAS_MORE(__VA_ARGS__), DEFER(ADD_PTR_PTR_COMMA)(__VA_ARGS__), )
#define ADD_PTR_PTR_COMMA(...)       , DEFER(ADD_PTR_PTR_IMPL)(__VA_ARGS__)
#define ADD_POINTER_TO_POINTERS(...) EVAL(ADD_PTR_PTR_IMPL(__VA_ARGS__))

template <typename T>
concept Ptr = std::is_pointer_v<T>;
template <typename T>
concept Ref = std::is_lvalue_reference_v<T>;

template <typename T, typename... Ts>
concept Every = (std::is_same_v<T, Ts> && ...);

template <typename T>
concept Str = std::is_convertible_v<T, std::string_view>;

template <typename T>
concept StrSource = std::is_same_v<T, std::string> || std::is_same_v<T, std::string_view> ||
                    std::is_same_v<T, const char*>;

template <typename T, typename = void>
struct ensure_pointer {
  using type = T*;
};

template <typename T>
concept IsTrivial = std::is_trivially_default_constructible_v<T> && std::is_trivially_copyable_v<T>;

template <typename R>
struct function_traits;

template <typename... Args>
struct param_count;

// Specialization to count parameters
template <typename First, typename... Rest>
struct param_count<First, Rest...> {
  static const std::size_t value = 1 + param_count<Rest...>::value; // Count the first and recurse
};
// Base case for zero parameters
template <>
struct param_count<> {
  static const std::size_t value = 0;
  ; // No parameters
};
// Base template for function parameter count
template <typename T>
struct function_traits;

// Specialization for function pointers
template <typename Ret, typename... Args>
struct function_traits<Ret (*)(Args...)> {
  static const std::size_t arity = sizeof...(Args);
};

template <typename T, typename S>
concept Comparable = requires(T t, S s) {
  { t < s } -> std::convertible_to<bool>;
  { s < t } -> std::convertible_to<bool>;
  { t == s } -> std::convertible_to<bool>;
  { s == t } -> std::convertible_to<bool>;
};

template <typename From, typename To>
concept convertible_t = std::is_convertible_v<From, To>;

template <typename T>
concept pointer_t = std::is_pointer_v<T>;

template <typename T>
concept not_pointer_t = !std::is_pointer_v<T>;

template <template <typename...> class C, typename... Ts>
auto is_base_of_template_t_impl(const C<Ts...>*) -> std::true_type;

template <template <typename...> class C>
auto is_base_of_template_t_impl(...) -> std::false_type;

template <typename T, template <typename...> class C>
using is_base_of_template_t = decltype(is_base_of_template_t_impl<C>(std::declval<T*>()));

template <typename T, typename... Args>
concept constructible_t = std::is_constructible_v<T, Args...>;

template <typename T, typename V, std::size_t... I>
constexpr bool is_one_of_variant(std::index_sequence<I...>) {
  return (std::is_same_v<T, std::variant_alternative_t<I, V>> || ...);
}

template <typename T>
class Base {};

template <typename T>
class Derived {};

template <typename T, template <typename> class BaseTemplate>
concept DerivedFromTemplate = std::is_base_of_v<BaseTemplate<typename T::value_type>, T>;

template <typename Variant, template <typename> class Trait>
struct check_variant_compliance;

// Primary template: Assume T is not hashable
template <typename T, typename = void>
struct is_hashable : std::false_type {};

// Specialization: Check if std::hash<T> is valid
template <typename T>
struct is_hashable<T, std::void_t<decltype(std::hash<T>{}(std::declval<T>()))>> : std::true_type {};

// Helper alias for easier usage
template <typename T>
inline constexpr bool is_hashable_v = is_hashable<T>::value;

template <typename T>
concept Hashable = requires(T t) {
  { std::hash<T>{}(t) };
};

template <typename T>
concept IsSmartOrRawPointer = requires(T ptr) {
  // Raw pointer check
  { std::is_pointer_v<T> } -> std::convertible_to<bool>;

  // // Unique pointer check
  // requires std::is_same_v<T, std::unique_ptr<typename T::element_type>>;

  // Shared pointer check
  requires std::is_same_v<T, std::shared_ptr<typename T::element_type>>;
};

template <typename T>
concept ScalarLike = requires(T t) {
  requires std::ranges::range<T>;
  std::ranges::range_size_t<T>() == 1;
  typename std::remove_cvref_t<decltype(t)>::value_type;
};

template <typename T>
concept PairLike = requires(T t) {
  requires std::ranges::range<T>;
  typename std::remove_cvref_t<decltype(t)>::first_type;
  typename std::remove_cvref_t<decltype(t)>::second_type;
};

template <typename T>
concept EntryLike = requires(T t) {
  requires std::ranges::range<T>;
  typename std::remove_cvref_t<decltype(t)>::key_type;
  typename std::remove_cvref_t<decltype(t)>::mapped_type;
};

template <typename Func, typename... Args>
struct get_return_type : std::invoke_result_t<Func, Args...> {};

template <typename Func, typename... Args>
concept ReturnsVoid = std::is_same_v<get_return_type<Func, Args...>, void>;

template <typename T>
struct OptionalChecker {
  using type                        = std::remove_cv_t<std::remove_reference_t<T>>;

  static constexpr bool is_optional = requires(type opt) {
    { opt.has_value() } -> std::convertible_to<bool>;
    { opt.value() } -> std::same_as<typename type::value_type&>;
  };

  static constexpr bool is_empty(const T& opt) {
    if constexpr (is_optional) {
      return !opt.has_value();
    } else {
      return false;
    }
  }
};
template <typename Base, typename Derived>
struct polymorphic_traits {
  static constexpr bool is_base_polymorphic    = std::is_polymorphic_v<Base>;
  static constexpr bool is_derived_polymorphic = std::is_polymorphic_v<Derived>;
  static constexpr bool is_base_of             = std::is_base_of_v<Base, Derived>;
  static constexpr bool is_convertible         = std::is_convertible_v<Derived*, Base*>;
  static constexpr bool value =
      is_base_polymorphic && is_derived_polymorphic && is_base_of && is_convertible;
};
#define DERIVE_OK(BASE, DERIVED) static_assert(polymorphic_traits<BASE, DERIVED>::value);
