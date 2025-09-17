#pragma once

#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cxxabi.h>
#include <exception>
#include <magic_enum/magic_enum_all.hpp>
#include <magic_enum/magic_enum_flags.hpp>
#include <ranges>
#include <type_traits>

#include "logging.hpp"
#include "macros.hpp"
#include "stl.hpp"
#include "traits.hpp"

#define DEFAULT_CASE \
  default:           \
    assert(false)
#define MAKE_GENERIC_FORMATTABLE(CONCEPT, FUNC)                   \
  template <CONCEPT T, typename CharT>                            \
  struct fmt::formatter<T, CharT> : fmt::formatter<string_view> { \
    template <typename Ctx>                                       \
    auto format(const T& t_obj, Ctx& ctx) const {                 \
      return formatter<string_view>::format(FUNC, ctx);           \
    }                                                             \
  };                                                              \
  template <CONCEPT T, typename CharT>                            \
  struct std::formatter<T, CharT> : std::formatter<string_view> { \
    template <typename Ctx>                                       \
    auto format(const T& t_obj, Ctx& ctx) const {                 \
      return std::formatter<string_view>::format(FUNC, ctx);      \
    }                                                             \
  };

#define MAKE_FORMATTABLE(TYPE, FUNC)                                \
  template <>                                                       \
  struct fmt::formatter<TYPE> : fmt::formatter<string_view> {       \
    template <typename Ctx>                                         \
    auto format(const TYPE& loc, Ctx& ctx) const {                  \
      return fmt::formatter<string_view>::format(FUNC, ctx);        \
    }                                                               \
  };                                                                \
  template <>                                                       \
  struct std::formatter<TYPE, char> : std::formatter<string_view> { \
    template <typename Ctx>                                         \
    auto format(const TYPE& loc, Ctx& ctx) const {                  \
      return std::formatter<string_view>::format(FUNC, ctx);        \
    }                                                               \
  };
namespace cmm {

enum class comparison_t : uint8_t;

template <typename E>
concept scoped_enum = std::is_scoped_enum_v<E>;

template <scoped_enum E>
struct enumeration;

using std::string_view;

enum class compilation_error_t : uint8_t;
struct location;

#ifndef SAVE_PREPROCESSED
#  define SAVE_PREPROCESSED 0
#endif
#ifndef SAVE_ASSEMBLY
#  define SAVE_ASSEMBLY 0
#endif

constexpr static uint8_t DATASIZE      = 8;
constexpr static uint8_t MAX_ARGUMENTS = 6;
constexpr static uint8_t WORD_LEN      = DATASIZE; // bytes
constexpr static uint8_t MEM_ADDR_LEN  = WORD_LEN; // bytes

struct displayable {
  virtual ~displayable() = default;
  [[nodiscard]] constexpr virtual std::string repr() const { return string(); }
  [[nodiscard]] constexpr virtual std::string string() const = 0;
  explicit constexpr operator std::string() const { return string(); };
};

template <typename T>
concept Displayable = requires(T t) {
  { t.string() } -> stringish;
};

template <typename T>
concept DisplayablePtr = requires(T t) {
  { t->string() } -> stringish;
};

template <std::ranges::range T>
struct formattable_range {
  using value_type = std::ranges::range_value_t<T>;

  formattable_range(T*);

  template <class Delim>
  [[nodiscard]] constexpr std::string join(Delim&&) const;
  auto element_merger() const;

private:
  T* m_range;
};

} // namespace cmm

// Specialization for cmm::formattable itself
MAKE_GENERIC_FORMATTABLE(cmm::Displayable, t_obj.string());
MAKE_GENERIC_FORMATTABLE(cmm::DisplayablePtr, t_obj ? t_obj->string() : "No contained value");
MAKE_GENERIC_FORMATTABLE(cmm::scoped_enum, magic_enum::enum_name(t_obj));

namespace cmm {

template <typename T>
class reference : public std::reference_wrapper<T> {
public:
  using base_type = std::reference_wrapper<T>;
  using base_type::base_type;

  operator T&() noexcept { return this->get(); }
  operator const T&() const noexcept { return this->get(); }

  T* operator&() noexcept { return &this->get(); }
  const T* operator&() const noexcept { return &this->get(); }

  T* operator->() noexcept { return &this->get(); }
  const T* operator->() const noexcept { return &this->get(); }

  reference& operator=(const reference& other) {
    base_type::operator=(other);
    return *this;
  }

  reference& swap(T& other) {
    reference& stored = this->get();
    this              = other;
    return stored;
  }
};
template <scoped_enum E>
struct enumeration : displayable {
  using underlying_type = std::underlying_type_t<E>;
  constexpr enumeration(E e)
      : m_value(e) {};
  constexpr enumeration();
  template <scoped_enum From>
  constexpr explicit enumeration(From);
  constexpr enumeration& operator=(const enumeration&) = default;
  constexpr enumeration& operator=(enumeration&&)      = default;
  constexpr enumeration(enumeration&&)                 = default;
  constexpr enumeration(const enumeration&)            = default;
  constexpr ~enumeration() override                    = default;

  // Data
  static constexpr auto type_name() noexcept { return magic_enum::enum_type_name<E>(); }
  static constexpr auto count() noexcept { return magic_enum::enum_count<E>(); }
  [[nodiscard]] constexpr auto name() const noexcept { return magic_enum::enum_name<E>(m_value); };
  [[nodiscard]] constexpr auto value() const noexcept { return magic_enum::enum_integer(m_value); };
  [[nodiscard]] constexpr std::string string() const override;

  // Conversion
  constexpr operator underlying_type() const { return (underlying_type)value(); };
  template <typename To>
  constexpr To cast() const;

protected:
  E m_value;
};

#define CREATE_ENUMERATION(TYPE, ...)                                             \
  using value_type   = CONCAT(TYPE, _t);                                          \
  using element_type = TYPE;                                                      \
  using enumeration<value_type>::enumeration;                                     \
  using enum value_type;                                                          \
  DECLARE_VARS(__VA_ARGS__)                                                       \
  using member_types   = std::tuple<GET_TYPES(__VA_ARGS__)>;                      \
  using properties_map = magic_enum::containers::array<value_type, member_types>; \
  static_assert(std::is_constant_evaluated());                                    \
  [[nodiscard]] static constexpr const properties_map& properties_array();        \
  operator value_type() const { return self; }                                    \
  constexpr TYPE(value_type e)                                                    \
      : enumeration<value_type>(e),                                               \
        CTOR_ASSIGN(__VA_ARGS__) {}

#define CREATE_ENUMERATION_CLASS(TYPE, ...)                 \
  struct TYPE : public cmm::enumeration<CONCAT(TYPE, _t)> { \
    CREATE_ENUMERATION(TYPE, __VA_ARGS__)                   \
  };

#define BUILD_ENUMERATION(TYPE, ...)                                              \
  using value_type   = CONCAT(_, TYPE);                                           \
  using element_type = TYPE;                                                      \
  using enumeration<value_type>::enumeration;                                     \
  using enum value_type;                                                          \
  DECLARE_VARS(__VA_ARGS__)                                                       \
  using member_types   = std::tuple<GET_TYPES(__VA_ARGS__)>;                      \
  using properties_map = magic_enum::containers::array<value_type, member_types>; \
  [[nodiscard]] static constexpr const properties_map& properties_array();        \
  constexpr TYPE(value_type e)                                                    \
      : enumeration<value_type>(e),                                               \
        CTOR_ASSIGN(__VA_ARGS__) {}

#define BUILD_ENUMERATION_CLASS(TYPE, ...)                 \
  struct TYPE : public cmm::enumeration<CONCAT(_, TYPE)> { \
    BUILD_ENUMERATION(TYPE, __VA_ARGS__)                   \
  };

template <class T>
inline constexpr bool always_false = false;

template <typename Ret, typename... Ts>
struct overloaded : Ts... {
  using Ts::operator()...;
  template <typename T>
  consteval auto operator()(T) const = delete;
};

template <typename... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

struct location : displayable {
  uint32_t start{}, end{};

  constexpr location(uint32_t s, uint32_t o)
      : start(s),
        end(o) {}
  [[nodiscard]] std::string string() const override;

  friend bool operator==(const location& r, const location& l) {
    return r.start == l.start && r.end == l.end;
  }
  friend location operator+(const location& r, const location& l) {
    return {std::min(r.start, l.start), std::max(r.end, l.end)};
  }
  friend location& operator+=(location& r, const location& l) {
    r = r + l;
    return r;
  }
  friend std::optional<location> operator+(std::optional<location> lhs,
                                           std::optional<location> rhs) {
    if (!lhs && !rhs) {
      return std::nullopt;
    }
    if (!lhs) {
      return rhs;
    }

    if (!rhs) {
      return lhs;
    }

    return *lhs + *rhs;
  }

  friend std::optional<location> operator+(location lhs, std::optional<location> rhs) {
    if (rhs) {
      return lhs + *rhs;
    }
    return lhs;
  }
};

} // namespace cmm

MAKE_FORMATTABLE(std::optional<cmm::location>,
                 loc.has_value() ? loc.value().string() : "<No location>");

static_assert(std::formattable<std::optional<cmm::location>, char>);
static_assert(fmt::formattable<std::optional<cmm::location>, char>);

namespace cmm {
struct allocated {
  virtual ~allocated()                                 = default;
  [[nodiscard]] virtual cmm::location location() const = 0;
};

struct self_allocated : public allocated {
  self_allocated(cmm::location&&);
  self_allocated(const cmm::location&);

  [[nodiscard]] cmm::location location() const override { return m_location; }

private:
  cmm::location m_location;
};

template <typename T>
concept Allocated = std::is_base_of_v<allocated, std::remove_reference_t<T>>;

template <typename T>
concept AllocatedPtr =
    std::is_base_of_v<allocated, std::remove_pointer_t<T>> && std::is_pointer_v<T>;

template <typename... Ts>
constexpr bool EveryIsAllocated = (Allocated<Ts> && ...);

#define GET_LOC(p) (p == nullptr ? location() : p->location())

struct error : public std::exception {
  error()           = delete;
  ~error() override = default;

  error(std::string_view msg)
      : message(msg) {}
  template <typename... Args>
  error(std::format_string<Args...> t_fmt, Args&&... t_args)
      : message(std::format(t_fmt, std::forward<Args>(t_args)...)) {}

  [[nodiscard]] constexpr const char* what() const noexcept override { return message.data(); }

protected:
  std::string message;
};

enum class compilation_error_t : uint8_t {
  GENERIC,
  INVALID_CONTINUE,
  INVALID_BREAK,
  UNDECLARED_SYMBOL,
  ALREADY_DECLARED_SYMBOL,
  UNDEFINED_FUNCTION,
  ALREADY_DEFINED_FUNCTION,
  LABEL_IN_GLOBAL,
  RETURN_IN_GLOBAL,
  BAD_FUNCTION_CALL,
  WRONG_FUNCTION_ARGUMENT,
  UNEXPECTED_TOKEN,
  INCOMPATIBLE_TOKEN,
  REQUIRED_TYPE,
  TOO_MANY_TYPES,
  MISSING_ENTRY_POINT,
  NOT_BINDEABLE
};

BUILD_ENUMERATION_DATA_CLASS(compilation_error, std ::string_view, fmt, bool, located);

struct compilation_error : public cmm::error {
  std::optional<cmm::location> loc;

  compilation_error(const compilation_error_data&,
                    const std::string& str,
                    std::optional<location> l = std::nullopt)
      : error(str),
        loc(std::move(l)) {}
};

template <typename T>
concept is_allocated = requires(T t) {
  { t.location() } -> std::same_as<std::optional<cmm::location>>;
};

template <compilation_error_t Err, typename First, typename... Args>
[[noreturn]] void throw_error(First&& l, Args&&... args) {
  constexpr auto err = compilation_error_data(Err);
  if constexpr (is_allocated<First>) {
    throw compilation_error(
        err, std::format(err.fmt, l, std::forward<Args>(args)...), l.location());
  } else {
    throw compilation_error(err, std::format(err.fmt, l, std::forward<Args>(args)...));
  }
}

#define THROW(ERROR, ...)                                   \
  REGISTER_ERROR("Throwed {}", compilation_error_t::ERROR); \
  throw_error<compilation_error_t::ERROR>(__VA_ARGS__);

inline uint8_t string_hash(const std::string& str) {
  unsigned long hash = 0;
  for (char c : str) {
    hash = hash * 31 + static_cast<unsigned char>(c); // Multiply by 31 and add the character
  }
  return hash;
}

class string_buffer {
public:
  using buffer_type                                  = std::stringstream;
  using reference_type                               = buffer_type&;
  using constant_type                                = const buffer_type;

  string_buffer(string_buffer&&) noexcept            = default;
  string_buffer& operator=(string_buffer&&) noexcept = default;
  virtual ~string_buffer()                           = default;
  string_buffer(const string_buffer&)                = delete;
  string_buffer& operator=(const string_buffer&)     = delete;

  string_buffer();
  string_buffer& operator<<(string_buffer&);
  string_buffer& operator<<(const std::string&);

  // Main generators
  template <size_t, typename... Args>
  constexpr string_buffer& write(std::format_string<Args...>, Args&&...) noexcept;
  template <size_t>
  constexpr string_buffer& newline() noexcept;

  // For delays
  void create();
  void save();
  void load();
  std::string dump();
  std::string flush();
  [[nodiscard]] std::string_view snapshot() const noexcept;

private:
  reference_type active() noexcept;
  [[nodiscard]] const buffer_type& active() const noexcept;
  stack<buffer_type> m_actives;
  stack<buffer_type> m_saved;
};

template <size_t IndentLvl = 0, typename... Args>
constexpr string_buffer& string_buffer::write(std::format_string<Args...> std_string,
                                              Args&&... args) noexcept {
  active() << std::format("{}{}",
                          std::string(IndentLvl * 2, ' '),
                          std::format(std_string, std::forward<Args>(args)...));
  return *this;
}

template <size_t Times = 1>
constexpr string_buffer& string_buffer::newline() noexcept {
  active() << '\n';
  if constexpr (Times > 1) {
    return newline<Times - 1>();
  } else {
    return *this;
  }
}

// static_assert(std::is_copy_constructible_v<vector<int>>);

#define transform_vector(IN, FN) IN | std::views::transform(FN) | std::ranges::to<std::vector>()

template <typename T, typename... Args>
concept is_constructible = requires(Args&&... args) {
  { T{std::forward<Args>(args)...} } -> std::same_as<T>;
  !std::is_abstract_v<T>;
};

template <typename T>
constexpr inline auto generate = [](T& t) { return t; };

#define GET_MEMBER(OBJ, MEMBER)                            \
  if constexpr (requires { OBJ.operator->(); }) {          \
    OBJ->MEMBER                                            \
  } else if constexpr (std::is_pointer_v<decltype(OBJ)>) { \
    OBJ->MEMBER                                            \
  } else {                                                 \
    OBJ.MEMBER                                             \
  }

template <typename... Ts> // (7)
struct overload : Ts... {
  using Ts::operator()...;
};
template <class... Ts>
overload(Ts...) -> overload<Ts...>;

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

enum class keyword_t : uint8_t { IF, WHILE, FOR, GOTO, BREAK, CONTINUE, RETURN };
enum class instruction_result_reg : uint8_t { NONE, LEFT, RIGHT, ACCUMULATOR };

enum class arg_t : uint8_t { NONE, LEFT, RIGHT, ACC, AUX1, AUX2 };

namespace assembly {
enum class flag_t : uint16_t {
  NONE      = 1 << 0,
  CARRY     = 1 << 1, // CF  a > b
  ZERO      = 1 << 2, // ZF  a = b
  SIGN      = 1 << 3, // SF  a < b
  OVERFLOW  = 1 << 4, // OF
  PARITY    = 1 << 5, // PF
  AUXILIARY = 1 << 6, // AF
  DIRECTION = 1 << 7, // DF
  INTERRUPT = 1 << 8, // IF

  // System Flags
  TRAP       = 1 << 9,  // TF
  ALIGNMENT  = 1 << 10, // AC
  VIRTUAL808 = 1 << 11, // VM
  RESUME     = 1 << 12, // RF
  NESTEDTASK = 1 << 13, // NT
};
}
using namespace magic_enum::bitwise_operators;

enum class value_category_t : uint8_t {
  LVALUE  = 1 << 1,
  PRVALUE = 1 << 2,
  XVALUE  = 1 << 3,
  GLVALUE = LVALUE | XVALUE,
  RVALUE  = PRVALUE | XVALUE,
};

enum class comparison_t : uint8_t { EQ, NE, LE, LT, GE, GT, U_LT, U_GT };
}; // namespace cmm

template <>
struct magic_enum::customize::enum_range<cmm::assembly::flag_t> {
  static constexpr bool is_flags = true;
};
template <>
struct magic_enum::customize::enum_range<cmm::value_category_t> {
  static constexpr bool is_flags = true;
};

namespace cmm {

inline bool is_value_category(value_category_t lhs, value_category_t rhs) {
  return magic_enum::enum_flags_test(lhs, rhs);
}

enum class operator_sign : uint8_t { SIGNED, UNSIGNED };

struct comparison_data : public enumeration<comparison_t> {
  BUILD_ENUMERATION_DATA(comparison,
                         comparison_t,
                         inverse,
                         assembly::flag_t,
                         flags,
                         operator_sign,
                         sign);
  [[nodiscard]] instruction_t jump() const;
  [[nodiscard]] instruction_t set() const;
};

enum class associativity_t : uint8_t { Either, L2R, R2L };
BUILD_ENUMERATION_DATA_CLASS(operator,
                             std::string_view,
                             repr,
                             uint8_t,
                             precedence,
                             associativity_t,
                             assoc,
                             std::optional<comparison_t>,
                             comparison)

BUILD_ENUMERATION_DATA_CLASS(instruction,
                             short,
                             n_params,
                             bool,
                             can_address_memory,
                             instruction_result_reg,
                             where);

constexpr auto to_type = [](const auto& v) {
  if constexpr (std::is_pointer_v<decltype(v)>) {
    return v->type;
  } else {
    return v.type;
  }
};
constexpr auto to_location   = [](const auto& v) { return v->location(); };
constexpr auto opt_to_value  = [](const auto& v) { return v.value(); };
constexpr auto opt_has_value = [](const auto& v) { return v.has_value(); };

template <Allocated N>
constexpr auto vector_to_location = [](std::vector<N> v) {
  return std::ranges::fold_left_first(v | std ::views ::transform([](const auto& s) {
                                        if constexpr (std::is_pointer_v<N>) {
                                          return s->location();
                                        } else if constexpr (!std::is_pointer_v<N>) {
                                          return s.location();
                                        }
                                      }),
                                      std::plus<>()) |
         TO_VEC;
};

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

inline std::string mangle_function(std::string id, std::vector<std::string> args, char delim) {
  std::vector<std::string> it{{id}};
  it.append_range(args);
  return it | std::views::join_with(delim) | std::ranges::to<std::string>();
}

} // namespace cmm

#include "common.inl"
