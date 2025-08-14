#pragma once

#include "common.hpp"
#include "macros.hpp"

#include <array>
#include <cstdint>
#include <functional>
#include <libassert/assert.hpp>
#include <magic_enum/magic_enum.hpp>
#include <magic_enum/magic_enum_containers.hpp>
#include <tuple>
#include <type_traits>
#include <utility>

#define NOT_IMPLEMENTED UNREACHABLE("Not implemented method");

namespace cmm {

template <typename... Args>
size_t hash_combine(const Args&... args) {
  size_t seed = 0;
  ((seed ^= typeid(args).hash_code() + 0x9e3779b9 + (seed << 6) + (seed >> 2)), ...);
  return seed;
}

enum class type_category_t : uint8_t {
  any_t = 0,
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

static_assert(std::is_default_constructible_v<type_category_t>);

using type_metadata_t =
    std::tuple<type_category_t, type_category_t, std::array<type_category_t, 4>>;

using type_metadata_store_t =
    std::array<type_metadata_t, magic_enum::enum_count<type_category_t>()>;

extern const type_metadata_store_t type_metadata_store;

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

struct type : public formattable {
  type_category_t category;
  uint8_t rank                               = 0;
  const type* underlying                     = nullptr;
  bool c                                     = false;
  bool v                                     = false;

  constexpr ~type() override                 = default;
  constexpr type(const type&)                = default;
  constexpr type& operator=(const type&)     = default;
  constexpr type(type&&) noexcept            = default;
  constexpr type& operator=(type&&) noexcept = default;

private:
  constexpr explicit type(type_category_t t)
      : category(t) {}
  constexpr type(type_category_t t, bool c, bool v)
      : category(t),
        c(c),
        v(v) {}
  constexpr type(type_category_t t, const type* u)
      : category(t),
        underlying(u) {}
  constexpr type(type_category_t t, const type* u, bool c, bool v)
      : category(t),
        underlying(u),
        c(c),
        v(v) {}
  constexpr type(type_category_t t, const type* u, size_t n, bool c, bool v)
      : category(t),
        underlying(u),
        rank(n),
        c(c),
        v(v) {}

public:
  bool operator==(const type& other) const {
    return category == other.category && rank == other.rank && *underlying == *other.underlying &&
           c == other.c && v == other.v;
  }

  [[nodiscard]] std::string format() const override;
  template <typename... Args>
  static const type& create(type_category_t, Args&&...);
  static const type& create_fundamental(type_category_t, bool = false, bool = false);
  static const type& create_pointer(const type*, bool, bool);
  static const type& create_lvalue(const type*, bool, bool);
  static const type& create_array(const type*, size_t, bool, bool);
  static const type& create_string(size_t, bool = false, bool = false);
};

using ptr_type = const type*;
using cr_type  = const type&;

struct type_index {
  type_category_t type;
  size_t value;

  type_index(type_category_t, size_t);
};

#define LVALUE_STATIC_TYPE(NAME, TYPE, CONST_, VOLATILE_, ...) \
  static const ptr_type CONCAT(NAME, _T) = &type::create_lvalue(TYPE, false, false);

#define STATIC_TYPE(NAME, CLS, CONST_, VOLATILE_) \
  static const ptr_type CONCAT(NAME, _T) = \
      &type::create_fundamental(type_category_t::CLS, CONST_, VOLATILE_);

STATIC_TYPE(VOID, void_t, false, false);
STATIC_TYPE(UINT, uint_t, false, false);
STATIC_TYPE(SINT, sint_t, false, false);
STATIC_TYPE(CHAR, char_t, false, false);
STATIC_TYPE(FLOAT, float_t, false, false);
// INDIR_STATIC_TYPE(INTPTR, pointer_t, INT_T, false, false)
LVALUE_STATIC_TYPE(SINTREF, SINT_T, false, false);

enum class mask_t : uint8_t {
  TYPE,
  REFERENCED_TYPE,
};

struct type_mask {
  static bool category(type_category_t);
};

// Implicit conversion from lvalue of int to int and vice versa

template <typename T>
struct result {
  result()
      : ok(false) {}
  result(const T& t)
      : ok(true),
        data(t) {}
  bool ok;
  T data;
  operator bool() const { return ok; }
};
result<ptr_type> is_assignable(cr_type from, cr_type to);

template <type_category_t C>
struct is_void : traits::false_type {};
template <>
struct is_void<type_category_t::void_t> : traits::true_type {};

struct is_const_v {
  static constexpr bool operator()(cr_type);
};
struct is_indirect_v {
  static constexpr bool operator()(cr_type);
};
struct is_reference_v {
  static constexpr bool operator()(cr_type);
};
type_metadata_t get_metadata_of(type_category_t);
// std::vector<type_category_t> all_children_of(type_category_t);
bool belongs_to(type_category_t, type_category_t);
} // namespace cmm
template <>
struct std::hash<cmm::type> {
  size_t operator()(const cmm::type& t) const noexcept {
    return cmm::hash_combine(t.category, t.rank, t.underlying, t.c, t.v);
  }
};
namespace cmm {
template <typename T>
struct predicate_element {
  T value;
  constexpr predicate_element(T&& v)
      : value(std::move(v)) {}
  constexpr predicate_element(const T& v)
      : value(v) {}
};

template <typename Ext, typename Cond, typename Value>
struct pred_holder {
  Ext ext;
  Cond cond;
  Value value;

  // Enhanced constructor with SFINAE and concept support
  template <typename E, typename C, typename Val>
    requires std::constructible_from<Ext, E> && std::constructible_from<Cond, C> &&
                 std::constructible_from<Value, Val>
  constexpr pred_holder(E&& e, C&& c, Val&& value)
      : ext(std::forward<E>(e)),
        cond(std::forward<C>(c)),
        value(std::forward<Val>(value)) {}

  // Improved invocation with more robust type handling
  template <typename Obj>
  constexpr auto operator()(Obj&& obj) const
      -> std::invoke_result_t<Cond, std::invoke_result_t<Ext, Obj>> {
    auto extracted = std::invoke(ext, std::forward<Obj>(obj));
    return std::invoke(cond, extracted, value);
  }
  template <typename T1, typename T2>
  constexpr auto operator()(T1&& obj, T2&& obj2) const
      -> std::invoke_result_t<Cond, std::invoke_result_t<Ext, T1>, T2> {
    auto extracted = std::invoke(ext, std::forward<T1>(obj), std::forward<T2>(obj2));
    return std::invoke(cond, extracted, value);
  }
  // Additional utility methods
  template <typename NewCond>
  constexpr auto and_then(NewCond new_cond) const {
    return pred_holder{ext,
                       [old_cond = cond, new_cond](auto&& extracted, auto&& val) {
                         return old_cond(extracted, val) && new_cond(extracted, val);
                       },
                       value};
  }

  template <typename NewCond>
  constexpr auto or_else(NewCond new_cond) const {
    return pred_holder{ext,
                       [old_cond = cond, new_cond](auto&& extracted, auto&& val) {
                         return old_cond(extracted, val) || new_cond(extracted, val);
                       },
                       value};
  };
};

template <typename Action, typename... Preds>
struct type_matcher;

// Advanced Type Matcher Builder

enum class matcher_mode : uint8_t { COMPARISON, GENERATOR };
template <typename... Preds>
class type_matcher_builder {
private:
  std::tuple<Preds...> preds;

public:
  // Constructors with enhanced type deduction
  constexpr type_matcher_builder() = default;

  explicit constexpr type_matcher_builder(std::tuple<Preds...> p)
      : preds(std::move(p)) {}

  // Add a new predicate condition - stores extractor, condition, value as separate elements
  template <typename Ext, typename Cond, typename Value>
  [[nodiscard]] constexpr auto where(Ext&& ext, Cond&& cond, Value&& value) const {
    auto new_tuple = std::tuple_cat(preds,
                                    std::make_tuple(predicate_element{std::forward<Ext>(ext)},
                                                    predicate_element{std::forward<Cond>(cond)},
                                                    predicate_element{std::forward<Value>(value)}));
    return type_matcher_builder<Preds...,
                                predicate_element<std::decay_t<Ext>>,
                                predicate_element<std::decay_t<Cond>>,
                                predicate_element<std::decay_t<Value>>>(new_tuple);
  }

  template <typename Action>
  [[nodiscard]] constexpr auto then(Action&& action) const {
    return type_matcher<std::decay_t<Action>, Preds...>(std::forward<Action>(action), preds);
  }

  // Create a matcher without action (for filtering/matching only)
  [[nodiscard]] constexpr auto build() const { return type_matcher<void, Preds...>(preds); }

  // Utility to get current predicate count
  static constexpr size_t predicate_count = sizeof...(Preds);
};

template <typename Action, typename... Preds> class type_matcher {
private:
  [[no_unique_address]] Action action;
  std::tuple<Preds...> preds;

public:
  // Constructor with action
  template <typename A>
    requires(!std::is_same_v<Action, void>)
  constexpr type_matcher(A&& a, std::tuple<Preds...> p)
      : action(std::forward<A>(a)),
        preds(std::move(p)) {}

  // Constructor without action (for matching only)
  template <typename = void>
    requires std::is_same_v<Action, void>
  constexpr type_matcher(std::tuple<Preds...> p)
      : preds(std::move(p)) {}

  // Compare two
  template <typename T1, typename T2>
  constexpr bool compare(T1&& t1, T2&& t2) const {
    return process_groups<0>(std::forward<T1>(t1), std::forward<T2>(t2));
  }

  template <typename Obj>
  constexpr bool match(Obj&& obj) const {
    static_assert(sizeof...(Preds) % 3 == 0,
                  "Predicates must be in groups of 3 (extractor, condition, value)");
    return process_groups<0>(std::forward<Obj>(obj));
  }

private:
  template <size_t I, typename Obj, typename Obj2>
  constexpr bool process_groups(Obj&&, Obj2&&) const {
    if constexpr (I + 2 < sizeof...(Preds)) {
      // Extract a group of 3 elements starting at index I
      const auto& extractor  = std::get<I>(preds).value;
      const auto& condition  = std::get<I + 1>(preds).value;
      const auto& extractor2 = std::get<I + 2>(preds).value;

      // Apply extractor to get property from object
      // auto extracted_prop  = std::invoke(extractor, std::forward<Obj>(obj));
      // auto extracted_prop2 = std::invoke(extractor2, std::forward<Obj2>(obj2));

      // Check condition with the extracted property and expected value
      // bool result = std::invoke(condition, extracted_prop, expected_value);
      //
      // Continue with next group (AND logic)
      //   if constexpr (I + 5 < sizeof...(Preds)) {
      //     return result && process_groups<I + 3>(std::forward<Obj>(obj));
      //   } else {
      //     return result;
      //   }
      // } else {
      //   return true; // No more groups to process
      return {};
    }
  }
  template <size_t I, typename Obj>
  constexpr bool process_groups(Obj&& obj) const {
    if constexpr (I + 2 < sizeof...(Preds)) {
      // Extract a group of 3 elements starting at index I
      const auto& extractor      = std::get<I>(preds).value;
      const auto& condition      = std::get<I + 1>(preds).value;
      const auto& expected_value = std::get<I + 2>(preds).value;

      // Apply extractor to get property from object
      auto extracted_prop = std::invoke(extractor, std::forward<Obj>(obj));

      // Check condition with the extracted property and expected value
      bool result = std::invoke(condition, extracted_prop, expected_value);

      // Continue with next group (AND logic)
      if constexpr (I + 5 < sizeof...(Preds)) {
        return result && process_groups<I + 3>(std::forward<Obj>(obj));
      } else {
        return result;
      }
    } else {
      return true; // No more groups to process
    }
  }

public:
  // Apply action to matching elements in container
  template <typename Container>
    requires(!std::is_same_v<Action, void>)
  auto apply(Container&& container) const {
    using elem_t          = std::decay_t<decltype(*std::begin(container))>;
    using invoke_result_t = std::invoke_result_t<Action, elem_t>;

    if constexpr (std::is_void_v<invoke_result_t>) {
      // Side-effect based application
      for (auto& elem : container) {
        if (match(elem)) {
          std::invoke(action, elem);
        }
      }
    } else {
      // Result collection
      std::vector<invoke_result_t> results;
      for (auto& elem : container) {
        if (match(elem)) {
          results.push_back(std::invoke(action, elem));
        }
      }
      return results;
    }
  }

  // Filter container to matching elements
  template <typename Container>
  auto filter(Container&& container) const {
    using elem_t = std::decay_t<decltype(*std::begin(container))>;
    std::vector<elem_t> filtered;

    for (const auto& elem : container) {
      if (matches(elem)) {
        filtered.push_back(elem);
      }
    }
    return filtered;
  }

  // Chain with new action
  template <typename NextAction>
    requires(!std::is_same_v<Action, void>)
  auto then(NextAction&& next_action) const {
    return type_matcher<std::decay_t<NextAction>, Preds...>(std::forward<NextAction>(next_action),
                                                            preds);
  }

  // Add more predicates
  template <typename Ext, typename Cond, typename Value>
  auto where(Ext&& ext, Cond&& cond, Value&& value) const {
    using new_pred_t = pred_holder<std::decay_t<Ext>, std::decay_t<Cond>, std::decay_t<Value>>;
    auto new_pred =
        new_pred_t{std::forward<Ext>(ext), std::forward<Cond>(cond), std::forward<Value>(value)};
    auto new_tuple = std::tuple_cat(preds, std::make_tuple(new_pred));
    return type_matcher<Action, Preds..., new_pred_t>(action, new_tuple);
  }
};

// Convenience function to start building a matcher
constexpr auto make_matcher() { return type_matcher_builder<>(); }

#define CREATE_FN(FUNC, T1, ARG1, T2, ARG2) \
  [](const T1& ARG1, const T2& ARG2) { return FUNC(ARG1, ARG2); };
namespace extractors {
  constexpr static auto GET            = [](cr_type t) { return t; };
  constexpr static auto GET_RANK       = [](cr_type t) { return t.rank; };
  constexpr static auto GET_UNDERLYING = [](cr_type t) { return t.underlying; };
  constexpr static auto GET_CATEGORY   = [](cr_type t) { return t.category; };
  constexpr static auto GET_CONST      = [](cr_type t) { return t.c; };
  constexpr static auto GET_VOLATILE   = [](cr_type t) { return t.v; };
} // namespace extractors

namespace conditions {
  constexpr static auto EQUALS     = std::equal_to{};
  constexpr static auto NOT_EQUALS = std::not_equal_to{};
  constexpr static auto LESS       = std::less{};
  constexpr static auto GREATER    = std::greater{};
  constexpr static auto BELONGS_TO =
      CREATE_FN(belongs_to, type_category_t, t, type_category_t, cat);
}; // namespace conditions
namespace processors {
  template <type_category_t N>
  constexpr static auto GENERATE_TYPE      = [](cr_type) -> type { return type::create(N); };
  constexpr static auto EXTRACT_UNDERLYING = [](cr_type t) -> type { return *t.underlying; };
  constexpr static auto COPY               = [](cr_type t) -> type { return t; };
  constexpr static auto WRAP_REF           = [](cr_type t) -> type {
    return type::create_lvalue(&t, t.c, t.v);
  };
}; // namespace processors
namespace conversions {
  enum class generator_t : uint8_t { EVERY };
  using namespace conditions;
  using namespace extractors;
  using namespace processors;
  constexpr static auto TO_BOOL =
      make_matcher().where(GET_CATEGORY, BELONGS_TO, type_category_t::fundamental_t).then([]() {
        return type::create(type_category_t::bool_t);
      });
  constexpr static auto LVALUE_TO_RVALUE =
      type_matcher_builder()
          .where(GET_CATEGORY, EQUALS, type_category_t::lvalue_ref_t)
          .where(GET_CONST, NOT_EQUALS, true)
          .then(COPY);
  constexpr static auto CONST_LVALUE =
      type_matcher_builder()
          .where(GET_CATEGORY, EQUALS, type_category_t::lvalue_ref_t)
          .where(GET_CONST, EQUALS, true)
          .then(COPY);

  constexpr auto static conversions = std::make_tuple(TO_BOOL, LVALUE_TO_RVALUE, CONST_LVALUE);
  constexpr bool is_convertible(ptr_type from, ptr_type to) {
    return std::apply([&from, &to](auto&&... args) { return (args.compare(from, to) && ...); },
                      conversions::conversions);
  }
  constexpr std::vector<ptr_type> get_convertibles(ptr_type) {
    NOT_IMPLEMENTED;
    return {};
    // return std::apply([&from, &to](auto&&... args) { return (); }, conversions::conversions);
  }
  constexpr std::vector<ptr_type> get_convertible_types(ptr_type) {
    NOT_IMPLEMENTED;
    return {};
    // return std::apply([&from, &to](auto&&... args) { return (); }, conversions::conversions);
  }

  // constexpr static auto TO_BOOL = std::make_tuple();
} // namespace conversions

} // namespace cmm
#include "types.inl"
