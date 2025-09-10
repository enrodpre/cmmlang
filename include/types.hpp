#pragma once

#include <array>
#include <cassert>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <format>
#include <functional>
#include <initializer_list>
#include <magic_enum/magic_enum.hpp>
#include <magic_enum/magic_enum_containers.hpp>
#include <optional>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

#include "common.hpp"
#include "macros.hpp"

#define BASIC_T(TYPE) cmm::types::global().make(cmm::types::core_t::TYPE)
#define VOID_T        BASIC_T(void_t)
#define BOOL_T        BASIC_T(bool_t)
#define CHAR_T        BASIC_T(char_t)
#define SINT_T        BASIC_T(sint_t)
#define UINT_T        BASIC_T(uint_t)
#define FLOAT_T       BASIC_T(float_t)

#define REF_T(TYPE) types::add_lvalue_reference(BASIC_T(TYPE))

#define UINTREF_T  REF_T(uint_t)
#define SINTREF_T  REF_T(sint_t)
#define FLOATREF_T REF_T(float_t)

namespace cmm::types {

using enum_type = uint16_t;
enum class cv_qualification_t : uint8_t {
  NONE           = 0,
  CONST          = 1 << 0,
  VOLATILE       = 1 << 1,
  CONST_VOLATILE = (1 << 0) + (1 << 1)
};

enum class group_t : enum_type {
  any_t = 0,
  fundamental_t,
  arithmetic_t,
  integral_t,
  compound_t,
  indirection_t,
  reference_t,
  enum_t,
};
enum class core_t : enum_type {
  void_t = magic_enum::enum_count<group_t>(),
  nullptr_t,
  bool_t,
  char_t,
  uint_t,
  sint_t,
  float_t,
  scoped_enum_t,
  unscoped_enum_t,
  class_t
};
enum class layer_t : enum_type {
  lvalue_ref_t = magic_enum::enum_count<group_t>() + magic_enum::enum_count<core_t>(),
  rvalue_ref_t,
  pointer_t,
  array_t,
  function_t,
};

template <typename L, typename... R>
concept is_any = (std::is_same_v<L, R> || ...);

template <auto V, typename U>
inline constexpr bool is_value_of_enum_v = std::is_enum_v<U> && std::same_as<decltype(V), U>;

// variadic helper over many enum types
template <auto V, typename... Enums>
inline constexpr bool is_value_in_enums_v = (is_value_of_enum_v<V, Enums> || ...);

template <auto V, typename... Es>
struct enum_type_of {
  using types = std::tuple<std::conditional_t<magic_enum::enum_contains<Es>(V), void, Es>...>;
  static_assert(std::tuple_size_v<types>() == 1);
  using type = std::tuple_element_t<0, types>();
};

template <auto V, typename... Es>
using enum_type_of_t = enum_type_of<V, Es...>::type;

template <typename... Es, typename U>
constexpr bool value_in_any_enum(U v) {
  return (
      (magic_enum::enum_cast<Es>(static_cast<magic_enum::underlying_type_t<Es>>(v)).has_value()) ||
      ...);
}
template <typename... Enums>
consteval std::string_view available_enum_names() {
  // Return first one; real diagnostic will list all in assert below.
  return ((magic_enum::enum_type_name<Enums>()), ...);
}

template <typename T>
concept uint_comparable_t = std::convertible_to<T, uint64_t>;

struct category_data;

template <typename... Ts>
  requires(sizeof...(Ts) > 0)
using first_of = std::tuple_element_t<0, std::tuple<Ts...>>;

template <typename... Enums>
concept similar_enums = requires {
  typename first_of<Enums...>;
  requires(scoped_enum<Enums> && ...);
  (std::same_as<magic_enum::underlying_type_t<first_of<Enums>>,
                magic_enum::underlying_type_t<Enums>> &&
   ...);
};

template <typename L, typename R>
concept Comparable = (std::convertible_to<L, uint64_t> && std::convertible_to<R, uint64_t>);

template <typename T>
using rm_ref = std::remove_reference<T>;

template <similar_enums... Enums>
struct enum_union {
  using enum_types      = std::tuple<Enums...>;
  using first_enum      = std::tuple_element_t<0, std::tuple<Enums...>>;
  using underlying_type = std::common_type_t<std::underlying_type_t<Enums>...>;

  template <underlying_type V>
  struct enum_type {
    using maybe = typename enum_type_of<V, Enums...>::type;
    static_assert(!std::is_void_v<maybe>,
                  "enum_union::enum_type_t_strict: constant value does not belong to any provided"
                  " enum types");
    using type = maybe;
  };

  template <underlying_type V>
  using enum_type_t = typename enum_type<V>::type;

  DEFAULT_CLASS(enum_union);
  template <typename Enum>
    requires(std::same_as<Enum, Enums> || ...)
  constexpr enum_union(Enum e)
      : m_value(magic_enum::enum_integer(e)) {}
  constexpr enum_union(underlying_type e)
      : m_value(e) {}
  template <typename Enum>
    requires is_any<Enum, Enums...>
  constexpr enum_union& operator=(Enum e) {
    m_value = magic_enum::enum_integer(e);
    return *this;
  }

  template <underlying_type V>
  consteval enum_union(std::integral_constant<underlying_type, V>) {
    // using T = enum_type_t_strict<V>; // enforce valid enum type
    // m_value = V;
  }

  constexpr const category_data* operator->() const;
  constexpr underlying_type value() const noexcept { return m_value; } // validity / membership
  // constexpr explicit operator underlying_type() const noexcept { return value(); }
  constexpr bool valid() const noexcept { return value_in_any_enum<Enums...>(m_value); }
  template <scoped_enum T>
    requires(std::is_same_v<T, Enums> || ...)
  operator T() const {
    return magic_enum::enum_cast<enum_type_t>(m_value);
  }
  constexpr auto operator<=>(const enum_union& other) const = default;
  constexpr bool operator==(const enum_union& other) const  = default;
  constexpr std::string_view enum_type_name() const noexcept {
    return magic_enum::enum_type_name<enum_type_t>();
  }
  template <scoped_enum E>
  constexpr std::optional<E> as() const noexcept {
    return magic_enum::enum_cast<E>(m_value);
  }

  // enumerator name (if you need it)
  constexpr auto repr() const noexcept { return magic_enum::enum_name<enum_type_t>(m_value); }
  constexpr auto string() const noexcept { return magic_enum::enum_name<enum_type_t>(m_value); }

  constexpr static size_t size() noexcept { return ((magic_enum::enum_count<Enums>()) + ...); }

private:
  const underlying_type m_value{};
};

template <typename EnumType, typename... EnumTypes>
constexpr auto operator<=>(EnumType e, const enum_union<EnumTypes...>& eu) {
  return std::to_underlying(e) <=> eu.value();
}

template <typename EnumType, typename... EnumTypes>
constexpr bool operator==(EnumType e, const enum_union<EnumTypes...>& eu) {
  return std::to_underlying(e) == eu.value();
}
using category_t = enum_union<group_t, core_t, layer_t>;
static_assert(std::convertible_to<category_t, group_t>);
static_assert(std::convertible_to<core_t, category_t>);
static_assert(std::equality_comparable<category_t::underlying_type>);
template <auto V>
concept CategoryValue = is_value_in_enums_v<V, group_t, core_t, layer_t>;

template <auto V, typename... Enums>
concept EnumValue = ((magic_enum::enum_contains<Enums>(V) || ...));

static_assert(CategoryValue<layer_t::lvalue_ref_t>);
// helper to test if an integral value matches any enum in Union
struct category_data {
  using UT = category_t::underlying_type; // Replace with category_t::underlying_type
  UT self{};
  UT parent{};
  std::array<UT, 4> children{};

  constexpr category_data()                                    = default;
  constexpr category_data(const category_data&)                = default;
  constexpr category_data& operator=(const category_data&)     = delete;
  constexpr category_data(category_data&&) noexcept            = default;
  constexpr category_data& operator=(category_data&&) noexcept = default;
  constexpr category_data(UT s, UT p, std::array<UT, 4> c) noexcept
      : self(s),
        parent(p),
        children(c) {}
  // remove/avoid the template ctors unless you need them
};

template <auto Self, auto Parent, auto... Children>
consteval category_data MakeData() {
  // build children array from Children... (pad with zeros)
  constexpr std::size_t N = sizeof...(Children);

  // pack children into a constexpr array of UT
  constexpr std::array<typename category_data::UT, N> packed = {
      static_cast<typename category_data::UT>(Children)...};

  std::array<typename category_data::UT, 4> arr{};
  for (std::size_t i = 0; i < 4 && i < N; ++i) {
    arr[i] = packed[i];
    arr[i] = static_cast<category_data::UT>(packed[i]);
  }
  return category_data{static_cast<typename category_data::UT>(Self),
                       static_cast<typename category_data::UT>(Parent),
                       arr};
}
template <auto... Vs>
struct builder {
  static consteval auto make() { return category_data{Vs...}; } // namespace types
};

// ---------------------- standalone dense table builder that takes the functor NTTP
template <typename Union,
          typename Data,
          auto InitFunctor /* class-type NTTP with operator()(underlying) */>
struct enum_union_dense_table {
  using underlying_type                                = typename Union::underlying_type;
  static constexpr size_t Size                         = Union::size();

  inline static constexpr std::array<Data, Size> table = [] {
    std::array<Data, Size> out{};
    for (size_t i = 0; i < Size; ++i) {
      out[i] = InitFunctor(static_cast<underlying_type>(i));
    }
    return out;
  }();

  static constexpr const Data* get(underlying_type v) noexcept {
    if (v >= 0 && static_cast<std::size_t>(v) < Size) {
      return &table[static_cast<std::size_t>(v)];
    }
    return nullptr;
  }

  static constexpr const Data& at(underlying_type v) noexcept {
    return table[static_cast<std::size_t>(v)];
  }
};

// static_assert(std::is_assignable_v<category_t, category_t>);
static_assert(std::is_constructible_v<category_t, layer_t>);
static_assert(std::is_constructible_v<category_t, group_t>);
static_assert(std::is_constructible_v<category_t, core_t>);
static_assert(std::is_constructible_v<category_t>);
// static_assert(std::is_trivially_constructible_v<category_t>);
static_assert(std::is_trivially_constructible_v<category_t, category_t>);
static_assert(std::is_constructible_v<category_t, core_t>);
static_assert(std::is_assignable_v<category_t, layer_t>);
static_assert(std::is_assignable_v<category_t, layer_t>);
static_assert(std::is_assignable_v<category_t, layer_t>);
// static_assert(std::formattable<category_t, char>);
// static_assert(std::formattable<std::unique_ptr<category_t>, char>);
// static_assert(std::formattable<std::optional<category_t>, char>);
// static_assert(std::formattable<std::shared_ptr<category_t>, char>);
// static_assert(Displayable<category_t>);
static_assert(std::is_assignable_v<category_t, layer_t>);
// static_assert(!std::is_assignable_v<category_t, token_t>);
static_assert(std::is_trivially_copy_constructible_v<category_t>);

using namespace magic_enum::bitwise_operators;

} // namespace cmm::types

template <>
struct magic_enum::customize::enum_range<cmm::types::cv_qualification_t> {
  static constexpr bool is_flags = true;
};

namespace cmm {
namespace types {

struct core {
  core_t kind;
  std::string name; // "int", "float", "Mytype", ...

  core(core_t);
  bool operator==(const core& o) const noexcept { return kind == o.kind && name == o.name; }
};

struct layer {
  layer(layer_t t_layer, cv_qualification_t t_cv = {})
      : tag(t_layer),
        cv_qualifiers(t_cv) {}
  layer_t tag;
  cv_qualification_t cv_qualifiers{};
  size_t rank{};
  // Payload (only used by Array for now)
  bool operator==(const layer& o) const noexcept {
    return tag == o.tag && rank == o.rank && cv_qualifiers == o.cv_qualifiers;
  }
};

struct info;

struct type_id {
  using id_type       = uint32_t;
  constexpr type_id() = default;
  explicit constexpr type_id(id_type t_id)
      : m_id(t_id) {}
  const info* operator->() const;

  bool operator==(types::type_id other) const noexcept { return m_id == other.m_id; }
  constexpr bool is_valid() const { return m_id > 0; }

  id_type value() const { return m_id; }
  operator const info&() const;

private:
  id_type m_id = 0;
}; // 0 reserved for invalid

// Topmost layer first (index 0). Example: const int& => LRef, Const
struct info : displayable {
  using key_type = std::tuple<core, cv_qualification_t, cmm::stack<layer>>;

  info(const core&, cv_qualification_t, cmm::stack<layer>);
  // Base type
  types::core core;
  cv_qualification_t cv_qualifiers;

  // Indirections
  stack<layer> layers;

  // Helpers
  category_t categorize() const;
  std::string string() const override;
  cv_qualification_t& cvqual();
  const cv_qualification_t& cvqual() const;

  bool operator==(const info& other) const noexcept {
    return core == other.core && cv_qualifiers == other.cv_qualifiers && layers == other.layers;
  }
};

struct core_keyhash {
  size_t operator()(const info::key_type&) const noexcept;
};

struct core_keyeq {
  bool operator()(const info::key_type&, const info::key_type&) const noexcept;
};

using modifier_t = std::function<type_id(type_id)>;

struct modifier;

class manager {
public:
  types::type_id make(const types::core& c,
                      const stack<layer>& = {},
                      cv_qualification_t  = cv_qualification_t::NONE);
  const types::info& info(types::type_id) const;

  friend types::type_id;

  static const std::array<category_data, category_t::size()> s_category_data;

  inline static manager& instance() {
    static manager i;
    return i;
  }

  size_t types_size() const;

  // Makers of modifiers
  template <layer_t L>
  modifier make_layer_adder();
  template <auto L>
    requires(CategoryValue<L>)
  modifier make_layer_remover();
  template <cv_qualification_t L>
  modifier make_cv_adder();
  template <cv_qualification_t L>
  modifier make_cv_remover();

private:
  manager() = default;

  std::vector<types::info> m_nodes; // id -> node (id==index)
  std::unordered_map<info::key_type, uint32_t, core_keyhash, core_keyeq> m_idx;

  types::type_id get(types::info);
};

inline manager& global() { return manager::instance(); }

inline types::type_id make(const types::core& c,
                           std::initializer_list<layer> layers = {},
                           cv_qualification_t qual             = {}) {
  return global().make(c, stack<layer>(layers));
}

// Strip leading refs (LRef/RRef) and optionally leading top-level cv
struct StripPolicy {
  bool drop_ref    = true;
  bool drop_top_cv = true;
};

struct pattern;
struct unary_matcher;
struct modifier;

struct match_result {
  match_result(bool r)
      : ok(r) {}
  bool ok  = false;
  int cost = 0;
  // types::type_id canonical{};
  // match_result& operator&&(const match_result&);
  // match_result& operator||(const match_result&);
  // match_result& operator!();
  operator bool() const { return ok; }
};

template <typename... Args>
struct pattern_provider {
  pattern provide(Args...) const;
};

template <typename... Args>
constexpr unary_matcher provide(Args&&...);

struct unary_matcher : displayable {
  using unary_matcher_t = std::function<match_result(types::type_id)>;
  unary_matcher(std::string_view, unary_matcher_t);
  unary_matcher(const unary_matcher&)            = default;
  unary_matcher& operator=(const unary_matcher&) = default;
  match_result operator()(types::type_id) const;
  std::string string() const override;
  unary_matcher operator&&(const unary_matcher&) const;
  unary_matcher operator||(const unary_matcher&) const;
  unary_matcher operator!() const;

private:
  std::string m_desc;
  unary_matcher_t m_unary_matcher;
};

template <cv_qualification_t Cv>
constexpr unary_matcher is_cv_qualified();

template <auto V>
  requires CategoryValue<V>
constexpr unary_matcher is_category();

using unary_matcher_provider = std::function<unary_matcher(type_id)>;
inline unary_matcher any{"any", [](types::type_id) { return true; }};
inline unary_matcher is_nullptr      = is_category<core_t::nullptr_t>();
inline unary_matcher is_bool         = is_category<core_t::bool_t>();
inline unary_matcher is_floating     = is_category<core_t::float_t>();
inline unary_matcher is_unscoped     = is_category<core_t::unscoped_enum_t>();
inline unary_matcher is_integral     = is_category<group_t::integral_t>();
inline unary_matcher is_arithmetic   = is_category<group_t::arithmetic_t>();
inline unary_matcher is_compound     = is_category<group_t::compound_t>();
inline unary_matcher is_pointer      = is_category<layer_t::pointer_t>();
inline unary_matcher is_array        = is_category<layer_t::array_t>();
inline unary_matcher is_const        = is_cv_qualified<cv_qualification_t::CONST>();
inline unary_matcher is_volatile     = is_cv_qualified<cv_qualification_t::VOLATILE>();
inline unary_matcher is_lvalue       = is_category<layer_t::lvalue_ref_t>();
inline unary_matcher is_const_lvalue = is_lvalue && is_const;
inline unary_matcher is_rvalue       = is_category<layer_t::rvalue_ref_t>();
inline unary_matcher is_const_rvalue = is_rvalue && is_const;
inline unary_matcher is_reference    = is_lvalue || is_rvalue;
inline unary_matcher is_direct       = {"is_direct ",
                                        [](type_id t) -> match_result { return t->layers.empty(); }};

struct binary_matcher : displayable {
  using binary_matcher_t = std::function<match_result(types::type_id, type_id)>;
  binary_matcher(std::string_view, binary_matcher_t);
  binary_matcher(const binary_matcher&)            = default;
  binary_matcher& operator=(const binary_matcher&) = default;
  unary_matcher operator()(type_id) const;
  match_result operator()(type_id, type_id) const;
  std::string string() const override;

private:
  std::string m_desc;
  binary_matcher_t m_matcher;
};

inline unary_matcher_provider is_same = [](type_id lhs) -> unary_matcher {
  return {std::format("is same to {}", lhs->string()), [lhs](type_id rhs) { return lhs == rhs; }};
};

bool belongs_to(category_t, category_t);

struct modifier : cmm::displayable {
public:
  modifier(std::string_view desc, modifier_t mod)
      : m_desc(desc),
        m_modifier(mod) {}
  std::string string() const override { return m_desc; }
  bool is_modifiable(types::type_id) const;
  types::type_id operator()(types::type_id) const;

  //  m1 | m2 | m3  pipes the return of the last to the input of the next
  //  no matter what
  modifier operator|(const modifier&) const;
  // m1 ^ m2 ^ m3   feeds the specific type as the input of the first modifier
  // that can modify
  // modifier operator^(const modifier&) const;

protected:
  std::string m_desc;
  modifier_t m_modifier;
};

inline modifier operator&&(const types::unary_matcher&, const modifier&);

inline modifier type_identity        = {"type_identity", std::identity{}};
inline modifier add_lvalue_reference = global().make_layer_adder<layer_t::lvalue_ref_t>();
inline modifier add_rvalue_reference = global().make_layer_adder<layer_t::rvalue_ref_t>();
inline modifier add_pointer          = global().make_layer_adder<layer_t::pointer_t>();
inline modifier add_const            = global().make_cv_adder<cv_qualification_t::CONST>();
inline modifier add_volatile         = global().make_cv_adder<cv_qualification_t::VOLATILE>();
inline modifier add_cv               = add_const | add_volatile;

inline modifier remove_volatile      = global().make_cv_remover<cv_qualification_t::VOLATILE>();
inline modifier remove_const         = global().make_cv_remover<cv_qualification_t::CONST>();
inline modifier remove_cv            = remove_const | remove_volatile;
inline modifier remove_lvalue        = global().make_layer_remover<layer_t::lvalue_ref_t>();
inline modifier remove_rvalue        = global().make_layer_remover<layer_t::rvalue_ref_t>();
inline modifier remove_reference     = global().make_layer_remover<group_t::reference_t>();
inline modifier remove_cvref         = remove_cv | remove_reference;
inline modifier remove_extent        = global().make_layer_remover<layer_t::array_t>();

inline modifier decay                = {"decay", [](type_id t) -> type_id {
                           if (is_array(t)) {
                             return remove_extent(t);
                           }
                           return remove_cvref(t);
                         }};

inline constexpr modifier replace(type_id c) {
  return {"convert to", [c](type_id) -> type_id { return c; }};
}

} // namespace types
using types::type_id;
} // namespace cmm
template <>
struct std::formatter<cmm::types::type_id, char> : std::formatter<string_view> {
  template <typename Ctx>
  auto format(const cmm::types::type_id& t, Ctx& ctx) const {
    return std::formatter<string_view>::format(t->string(), ctx);
  }
};

#include "types.inl"
