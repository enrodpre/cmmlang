#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <functional>

#include <magic_enum/magic_enum.hpp>
#include <magic_enum/magic_enum_containers.hpp>
#include <string>
#include <type_traits>
#include <utility>

#include "common.hpp"
#include "macros.hpp"
#include "token.hpp"
#include <cassert>
#include <cstdint>
#include <functional>
#include <string>
#include <utility>
#include <vector>

namespace cmm {

namespace types {
enum class qualification_t : uint8_t { NONE = 0, CONST = 1, VOLATILE = 2, CONST_VOLATILE = 3 };

enum class group_t : uint8_t {
  any_t = 0,
  fundamental_t,
  arithmetic_t,
  integral_t,
  compound_t,
  indirection_t,
  reference_t,
  enum_t,
};
enum class core_t : uint8_t {
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
enum class layer_t : uint8_t {
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

#define ENUM_SIZE(ENUM) +magic_enum::enum_count<ENUM>()
#define ENUMS_SIZE(...) FOR_EACH_1(ENUM_SIZE, __VA_ARGS__)

#define TO_UNDERLYING(ENUM)
#define CHECK_SAME_UNDERLYING_TYPES(FIRST, ...) FOR_EACH_1(TO_UNDERLYING, __VA_ARGS__)

#define ENUM_UNION(NAME, CONCEPT, ...)                                                             \
  struct category_data;                                                                            \
  template <typename T>                                                                            \
  concept CONCEPT = is_any<T, __VA_ARGS__>;                                                        \
  template <auto V>                                                                                \
  concept CONCAT(Value, CONCEPT) = is_value_in_enums_v<V, __VA_ARGS__>;                            \
  struct NAME {                                                                                    \
    USING_ENUMS(__VA_ARGS__)                                                                       \
    using value_type                                = std::variant<__VA_ARGS__>;                   \
    using underlying_type                           = uint8_t;                                     \
    constexpr NAME()                                = default;                                     \
    constexpr ~NAME()                               = default;                                     \
    constexpr NAME(const NAME&)                     = default;                                     \
    constexpr NAME& operator=(const NAME&) noexcept = default;                                     \
    constexpr NAME& operator=(NAME&&) noexcept      = default;                                     \
                                                                                                   \
    template <CONCEPT Enum>                                                                        \
    constexpr NAME(Enum e)                                                                         \
        : m_value(e) {}                                                                            \
    template <CONCEPT Enum>                                                                        \
    constexpr NAME& operator=(Enum e) {                                                            \
      m_value = e;                                                                                 \
      return *this;                                                                                \
    }                                                                                              \
    constexpr category_data operator->() const;                                                    \
    constexpr operator underlying_type() const {                                                   \
      return m_value.visit([](auto held) -> magic_enum::underlying_type_t<decltype(held)> {        \
        return magic_enum::enum_integer(held);                                                     \
      });                                                                                          \
    }                                                                                              \
    constexpr bool operator==(NAME other) const noexcept {                                         \
      if (m_value.index() != other.m_value.index()) {                                              \
        return false;                                                                              \
      }                                                                                            \
      return std::visit(                                                                           \
          [](auto a, auto b) {                                                                     \
            if constexpr (std::is_same_v<std::decay_t<decltype(a)>, std::decay_t<decltype(b)>>) {  \
              return a == b;                                                                       \
            } else {                                                                               \
              return false;                                                                        \
            }                                                                                      \
          },                                                                                       \
          m_value,                                                                                 \
          other.m_value);                                                                          \
    }                                                                                              \
                                                                                                   \
    constexpr auto string() const {                                                                \
      return m_value.visit([](auto held) { return magic_enum::enum_name<decltype(held)>(held); }); \
    }                                                                                              \
    constexpr static auto size() { return ENUMS_SIZE(__VA_ARGS__); }                               \
                                                                                                   \
  private:                                                                                         \
    value_type m_value;                                                                            \
  }; // namespace types

ENUM_UNION(category_t, Category, group_t, core_t, layer_t);

struct category_data {
  using value_type = category_t::value_type;
  value_type self;
  value_type parent;
  std::array<value_type, 4> children;

  using datatypes  = std::tuple<value_type, value_type, std::array<value_type, 4>>;
  using properties = std::array<datatypes, category_t::size()>;
  static constexpr const properties& get_properties();
  constexpr category_data(const category_t::underlying_type i)
      : self(std::get<0>(get_properties()[i])),
        parent(std::get<1>(get_properties()[i])),
        children(std::get<2>(get_properties()[i])) {}
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
static_assert(std::formattable<category_t, char>);
static_assert(std::formattable<std::unique_ptr<category_t>, char>);
static_assert(std::formattable<std::optional<category_t>, char>);
static_assert(std::formattable<std::shared_ptr<category_t>, char>);
constexpr category_t c = core_t::unscoped_enum_t;
static_assert(Displayable<category_t>);
static_assert(std::is_assignable_v<category_t, layer_t>);
static_assert(!std::is_assignable_v<category_t, token_t>);
static_assert(std::is_trivially_copy_constructible_v<category_t>);
void a() {
  category_t::size();
  constexpr category_t cat = category_t::scoped_enum_t;
  static_assert(cat == core_t::scoped_enum_t);
  constexpr category_t category = layer_t::function_t;
  static_assert(category != core_t::scoped_enum_t);
  static_assert(ValueCategory<category_t::any_t>);
  static_assert(!ValueCategory<token_t::neq>);
  static_assert(sizeof(category_t) == 2);
}

using namespace magic_enum::bitwise_operators;

} // namespace types
} // namespace cmm

template <>
struct magic_enum::customize::enum_range<cmm::types::qualification_t> {
  static constexpr bool is_flags = true;
};

namespace cmm {
namespace types {

struct core {
  core_t kind;
  std::string name; // "int", "float", "Mytype", ...

  core(core_t);
  core(core_t, category_t);
  bool operator==(const core& o) const noexcept { return kind == o.kind && name == o.name; }
};

struct layer {
  layer(layer_t l)
      : tag(l) {}
  layer_t tag;
  qualification_t cv_qualifiers;
  size_t rank{};
  // Payload (only used by Array for now)
  bool operator==(const layer& o) const noexcept { return tag == o.tag && rank == o.rank; }
};

struct info;

struct type_id {
  uint32_t id = 0;
  const info* operator->() const;
  bool operator==(type_id other) const noexcept { return id == other.id; }
  types::info info() const;
  bool is_valid() const { return id != 0; }
}; // 0 reserved for invalid

category_t categorize(type_id);

// Topmost layer first (index 0). Example: const int& => LRef, Const
struct info : displayable {
  using key_type = std::pair<core, stack<layer>>;

  info(const core&, qualification_t, stack<layer>);
  // Base type
  core core;
  qualification_t cv_qualifiers;

  // Indirections
  stack<layer> layers;

  // Helpers
  category_t categorize() const;
  std::string string() const override { return core.name; }
  type_id id() const;
};

struct core_keyhash {
  size_t operator()(const info::key_type&) const noexcept;
};

struct core_keyeq {
  bool operator()(const info::key_type&, const info::key_type&) const noexcept;
};

class manager {
public:
  manager() {
    // m_nodes.push_back(info(core(core_t::, category_t::dummy_t), qualification_t::NONE, {}));
  }

  type_id make(const core& c,
               const stack<layer>& layers,
               qualification_t qual = qualification_t::NONE) {
    auto key = std::make_pair(c, layers);
    auto it  = m_idx.find(key);
    if (it != m_idx.end())
      return type_id{it->second};
    uint32_t id = static_cast<uint32_t>(m_nodes.size());
    m_nodes.emplace_back(c, qual, layers);
    m_idx.emplace(std::move(key), id);
    return type_id{id};
  }

  type_id get(info i) { return make(i.core, i.layers, i.cv_qualifiers); }

  const info& id(type_id t) const {
    assert(t.id < m_nodes.size());
    return m_nodes[t.id];
  }

  friend type_id;

private:
  std::vector<info> m_nodes; // id -> node (id==index)
  std::unordered_map<info::key_type, uint32_t, core_keyhash, core_keyeq> m_idx;
};

inline manager& arena() {
  static manager A;
  return A;
}

inline type_id make(const core& c,
                    std::initializer_list<layer> layers = {},
                    qualification_t qual                = {}) {
  return arena().make(c, stack<layer>(layers));
}

// ---------- Helpers to push/peel layers & normalize ----------

template <ScopedEnum Enum>
constexpr std::string enum_strip_t(Enum);
template <auto E>
constexpr std::string enum_strip_t();

// Strip leading refs (LRef/RRef) and optionally leading top-level cv
struct StripPolicy {
  bool drop_ref    = true;
  bool drop_top_cv = true;
};

template <core_t CoreT, auto Cat, qualification_t Cv, layer_t... Layers>
  requires(ValueCategory<Cat>)
struct builder {
  static type_id build() {
    core c(CoreT, Cat);
    stack<layer_t> layers;
    (layers.push(Layers), ...);
    return arena().make(c, layers, Cv);
  }
};

struct pattern;
struct matcher;
struct modifier;
struct conversor;

struct match_result {
  bool ok  = false;
  int cost = 0;
  type_id canonical{};
  match_result& operator&&(const match_result&);
  match_result& operator||(const match_result&);
  match_result& operator!();
  operator bool() const { return ok; }
};
struct pattern;

template <typename... Args>
struct pattern_provider {
  pattern provide(Args...) const;
};

template <typename... Args>
constexpr matcher provide(Args&&...);

struct matcher : displayable {
  using matcher_t = std::function<bool(type_id)>;
  matcher(std::string, matcher_t);
  matcher(const matcher&)            = default;
  matcher& operator=(const matcher&) = default;
  match_result operator()(type_id) const;
  std::string string() const override;
  matcher operator&&(const matcher&) const;
  matcher operator||(const matcher&) const;
  matcher operator!() const;

private:
  std::string m_desc;
  matcher_t m_matcher;
};

template <auto Cat>
  requires(ValueCategory<Cat>)
constexpr matcher is_category();
template <auto Cat>
  requires(ValueCategory<Cat>)
constexpr match_result is_category(type_id);

inline matcher is_const     = {"is const", [](type_id t) {
                             qualification_t qual = t->layers.empty()
                                                            ? t->cv_qualifiers
                                                            : t->layers.top().cv_qualifiers;
                             return qual == qualification_t::CONST ||
                                    qual == qualification_t::CONST_VOLATILE;
                           }};
inline matcher is_pointer   = is_category<category_t::pointer_t>();
inline matcher is_lvalue    = is_category<category_t::lvalue_ref_t>();
inline matcher is_rvalue    = is_category<category_t::lvalue_ref_t>();
inline matcher is_reference = is_lvalue || is_rvalue;

using children_t            = std::array<category_t, 4>;

// BUILD_ENUMERATION_DATA_CLASS(category, category_t, parent, children_t, child);

#define GROUP_TYPES                                                                   \
  type_t::fundamental_t, type_t::void_t, type_t::arithmetic_t, type_t::integral_t,    \
      type_t::compound_t, type_t::indirection_t, type_t::reference_t, type_t::enum_t,

#define INSTANCIABLE_TYPES()                                                          \
  type_t::nullptr_t, type_t::bool_t, type_t::char_t, type_t::uint_t, type_t::sint_t,  \
      type_t::float_t, type_t::lvalue_ref_t, type_t::rvalue_ref_t, type_t::pointer_t, \
      type_t::arrayinstance_data_t, type_t::function_t, type_t::scoped_enum_t,        \
      type_t::unscoped_enum_t, type_t::class_t

bool belongs_to(category_t, category_t);

struct modifier : cmm::displayable {
private:
  using modifier_t = std::function<info(info)>;

public:
  inline static modifier_t identity = [](info t) -> info { return t; };
  modifier()
      : m_desc("Identity"),
        m_modifier(identity) {}
  modifier(std::string desc, modifier_t mod)
      : m_desc(desc),
        m_modifier(mod) {}
  std::string string() const override { return m_desc; }
  type_id operator()(type_id) const;
  info operator()(info) const;
  modifier operator|(const modifier&) const;

protected:
  std::string m_desc;
  modifier_t m_modifier;
};

template <layer_t L>
modifier make_wrapper();
template <layer_t L>
modifier make_peeler();
template <qualification_t L>
modifier make_qualifier();

inline modifier add_lvalue_reference = make_wrapper<layer_t::lvalue_ref_t>();
inline modifier add_rvalue_reference = make_wrapper<layer_t::rvalue_ref_t>();
inline modifier add_const            = make_qualifier<qualification_t::CONST>();
inline modifier remove_lvalue        = make_peeler<layer_t::lvalue_ref_t>();
inline modifier remove_rvalue        = make_peeler<layer_t::rvalue_ref_t>();
inline modifier remove_reference     = make_peeler<layer_t::lvalue_ref_t>();

struct converter : public modifier {
  struct builder;

  type_id operator()(type_id) const;
  [[nodiscard]] bool is_convertible(type_id) const;

  [[nodiscard]] std::string string() const override { return m_desc; }

  friend builder;

private:
  type_id m_from;
  std::string m_desc;
};
extern std::vector<type_id> get_convertible_types(type_id);
extern bool is_convertible(type_id, type_id);

// #define make_belongs(NAME, CAT)                                                               \
//   inline static const matcher NAME(                                                           \
//       #NAME, [](type t) -> bool { return t && belongs_to(t->category, category_t::CAT); })
//
// #define make_is(NAME, CAT)                                                          \
//   inline static const matcher NAME(                                                 \
//       #NAME, [](type t) -> bool { return t && category_t::CAT == t->category; })
//
// #define make_matcher(NAME, COND)                                                      \
//   inline static const matcher NAME(#NAME, [](type lhs) -> bool { return (COND); })
//
// #define make_combined(NAME, COMBINED) inline static const matcher NAME(#NAME, COMBINED)
//
// namespace matchers {
// inline static const matcher any("any", [](type) { return true; });
// make_belongs(is_arithmetic, arithmetic_t);
// make_belongs(is_integral, integral_t);
// make_is(is_lvalue, lvalue_ref_t);
// make_is(is_rvalue, rvalue_ref_t);
// make_combined(is_ref, is_lvalue || is_rvalue);
// make_is(is_pointer, pointer_t);
// make_is(is_floating, float_t);
// make_is(is_unscoped, unscoped_enum_t);
// make_is(is_array, array_t);
// make_matcher(is_const, lhs->c);
// make_matcher(is_volatile, lhs->v);
// make_combined(is_const_lvalue, is_const&& is_lvalue);
//
// } // namespace matchers
//
// // A converter is just a restricter modifier
//
// struct type_comparer {};
//
// DECLARE_BUILDER(type_converter)
// BUILDER_STEP(name, std::string, m_desc)
// BUILDER_STEP_SETTER(from, type_matcher, m_from, std::make_shared<const type_matcher>(value))
// BUILDER_STEP(from, type, m_from)
// BUILDER_STEP_SETTER(to,
//                     type,
//                     m_modifier,
//                     type_modifier(std::format("to {}", value), [value](type) { return value;
//                     }))
// BUILDER_STEP(with, type_modifier, m_modifier)
// END_BUILDER()
//
// #define BINDING_CONVERSION(NAME, COND, WITH) \
//   inline const auto& NAME = type_converter::builder().name(#NAME).from(COND).with(WITH).build()
// #define NORMAL_CONVERSION(NAME, COND, TO)                                                   \
//   inline const auto& NAME = type_converter::builder().name(#NAME).from(COND).to(TO).build()
//
// namespace conversions {
// BINDING_CONVERSION(lvalue_to_rvalue,
//                    matchers::is_lvalue,
//                    modifiers::remove_reference | modifiers::add_rvalue_reference);
// // extern const type_converter nullptr_to_ptr;
// NORMAL_CONVERSION(any_to_bool,
//                   matchers::is_integral || matchers::is_pointer || matchers::is_unscoped ||
//                       matchers::is_floating,
//                   BOOL_T);
// // extern const type_converter bool_to_any;
// BINDING_CONVERSION(array_to_pointer, matchers::is_array, modifiers::decay);
//
// inline const std::array standard = {&any_to_bool, &lvalue_to_rvalue};
//
// }; // namespace conversions
//
} // namespace types

using type_id = types::type_id;
} // namespace cmm

// template <>
// struct std::hash<cmm::type> {
//   size_t operator()(const cmm::type& t) const noexcept {
//     return cmm::hash_combine(t.category, t.rank, t.underlying, t.c, t.v);
//   }
// };

template <>
struct std::formatter<cmm::types::type_id, char> : std::formatter<string_view> {
  template <typename Ctx>
  auto format(const cmm::types::type_id& t, Ctx& ctx) const {
    return std::formatter<string_view>::format(t->string(), ctx);
  }
};

#define VOID_T cmm::types::arena().

#include "types.inl"
