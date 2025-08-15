#pragma once

#include "fs.hpp"
#include "macros.hpp"
#include "os.hpp"
#include "traits.hpp"
#include <bits/version.h>
#include <concepts>
#include <cpptrace/utils.hpp>
#include <cstdint>
#include <exception>
#include <format>
#include <functional>
#include <libassert/assert.hpp>
#include <magic_enum/magic_enum_all.hpp>
#include <magic_enum/magic_enum_format.hpp>
#include <memory>
#include <mutex>
#include <ranges>
#include <string>
#include <sys/types.h>
#include <tuple>
#include <type_traits>
#include <typeindex>
#include <utility>

namespace cmm {

namespace log {
  enum class style_t : uint8_t {
    HEADER,
    BOLD,
    ERROR,
    NORMAL,
    RED,
    MAGENTA,
    YELLOW,
    GREEN,
    DARK_RED,
    WHITE_SMOKE,
    WHITE
  };

  template <typename T>
  constexpr std::string apply(T&&, style_t);

  enum class Level : uint8_t { NONE = 0, ERROR, WARN, INFO, DEBUG, TRACE };
} // namespace log
#ifndef LOG_LEVEL
  #define LOG_LEVEL TRACE_LEVEL
#endif

#define TRACE_LEVEL 5
#define DEBUG_LEVEL 4
#define INFO_LEVEL  3
#define WARN_LEVEL  2
#define ERROR_LEVEL 1
#define NONE_LEVEL  0

#define REGISTER_LOG(lvl, file, header_color, formatter_string, ...) \
  std::print(file, "[{}:{} {}] ", __FILE_NAME__, __LINE__, log::apply(lvl, header_color)); \
  std::println(file, formatter_string, ##__VA_ARGS__)

#if LOG_LEVEL >= ERROR_LEVEL
  #define REGISTER_ERROR(std_string, ...) \
    REGISTER_LOG(STRINGIZE_IMPL(ERROR), stderr, log::style_t::RED, std_string, ##__VA_ARGS__)
#else
  #define REGISTER_ERROR(std_string, ...)
#endif

#if LOG_LEVEL >= WARN_LEVEL
  #define REGISTER_WARN(std_string, ...) \
    REGISTER_LOG(STRINGIZE_IMPL(WARN), stdout, log::style_t::MAGENTA, std_string, ##__VA_ARGS__)
#else
  #define REGISTER_WARN(std_string, ...)
#endif

#if LOG_LEVEL >= INFO_LEVEL
  #define REGISTER_INFO(std_string, ...) \
    REGISTER_LOG(STRINGIZE_IMPL(INFO), stdout, log::style_t::GREEN, std_string, ##__VA_ARGS__)
#else
  #define REGISTER_INFO(std_string, ...)
#endif

#if LOG_LEVEL >= DEBUG_LEVEL
  #define REGISTER_DEBUG(std_string, ...) \
    REGISTER_LOG(STRINGIZE_IMPL(DEBUG), stdout, log::style_t::YELLOW, std_string, ##__VA_ARGS__)

#else
  #define REGISTER_DEBUG(std_string, ...)
#endif

#if LOG_LEVEL >= TRACE_LEVEL
  #if DEBUG_MEMORY
    #define MEMORY_TRACE(std_string, ...) \
      REGISTER_LOG( \
          STRINGIZE_IMPL(TRACE), stdout, std::color::white_smoke, std_string, ##__VA_ARGS__)
  #else
    #define MEMORY_TRACE(std_string, ...)
  #endif
  #define REGISTER_TRACE(std_string, ...) \
    REGISTER_LOG(STRINGIZE_IMPL(TRACE), stdout, log::style_t::WHITE, std_string, ##__VA_ARGS__)
#else
  #define REGISTER_TRACE(std_string, ...)
#endif

#ifndef SAVE_PREPROCESSED
  #define SAVE_PREPROCESSED 0
#endif
#ifndef SAVE_ASSEMBLY
  #define SAVE_ASSEMBLY 0
#endif

constexpr static uint8_t DATASIZE      = 8;
constexpr static uint8_t MAX_ARGUMENTS = 6;
constexpr static uint8_t WORD_LEN      = DATASIZE; // bytes
constexpr static uint8_t MEM_ADDR_LEN  = WORD_LEN; // bytes

using ushort                           = unsigned short;
using cstring                          = std::string_view;
using cr_string                        = const std::string&;

struct hashable {
  virtual ~hashable()                                = default;
  bool operator==(const hashable&) const             = default;
  [[nodiscard]] virtual size_t hash() const          = 0;
  [[nodiscard]] virtual std::type_index type() const = 0;
};

struct TypeErasedHash {
  size_t operator()(const std::unique_ptr<hashable>& obj) const { return obj ? obj->hash() : 0; }
};

// Custom equality function for type-erased objects

struct displayable {
  constexpr virtual ~displayable() = default;
  constexpr operator std::string() const;
  [[nodiscard]] virtual std::string repr() const { return string(); }
  [[nodiscard]] virtual std::string string() const = 0;
};

struct formattable {
  constexpr virtual ~formattable() = default;
  constexpr operator std::string() const;
  [[nodiscard]] virtual std::string format() const = 0;
  [[nodiscard]] virtual std::string repr(size_t = 0) const { return format(); }
};

#define STRING_IMPL(TYPE, stdstr, ...) \
  std::string TYPE::format() const { return std::format(stdstr, __VA_ARGS__); }

template <typename T>
concept Formattable =
    std::is_base_of_v<cmm::formattable, std::remove_cv_t<std::remove_reference_t<T>>>;

template <typename T>
concept Displayable =
    std::is_base_of_v<cmm::displayable, std::remove_cv_t<std::remove_reference_t<T>>>;

template <typename T>
concept FormattablePtr =
    std::is_base_of_v<cmm::formattable, std::remove_cv_t<std::remove_pointer_t<T>>>;

constexpr static inline auto format_func = [](const auto& _formattable) -> std::string {
  if constexpr (std::is_pointer_v<decltype(_formattable)>) {
    return _formattable->format();
  } else {
    return _formattable.format();
  }
};

template <std::ranges::range T>
struct formattable_range {
  using value_type = std::ranges::range_value_t<T>;

  formattable_range(T*);
  // static_assert(std::formattable<std::ranges::range_value_t<T>, char>);

  template <class Delim>
  [[nodiscard]] constexpr std::string join(Delim&&) const;
  auto element_merger() const;

private:
  T* m_range;
};

} // namespace cmm

// Specialization for cmm::formattable itself
template <cmm::Displayable T>
struct std::formatter<T, char> : std::formatter<string_view> {
  template <typename Ctx>
  auto format(const T& obj, Ctx& ctx) const {
    return std::formatter<string_view>::format(obj.string(), ctx);
  }
};

template <cmm::Displayable T>
struct std::formatter<T*, char> : std::formatter<string_view> {
  template <typename Ctx>
  auto format(const T* obj, Ctx& ctx) const {
    return std::formatter<string_view>::format(obj->string(), ctx);
  }
};

// Specialization for cmm::formattable itself
template <cmm::Formattable T>
struct std::formatter<T, char> : std::formatter<string_view> {
  template <typename Ctx>
  auto format(const T& obj, Ctx& ctx) const {
    return std::formatter<string_view>::format(obj.format(), ctx);
  }
};

template <cmm::Formattable T>
struct std::formatter<T*, char> : std::formatter<string_view> {
  template <typename Ctx>
  auto format(const T* obj, Ctx& ctx) const {
    return std::formatter<string_view>::format(obj->format(), ctx);
  }
};

namespace cmm {
template <typename T>
std::string type_name() {
  return cpptrace::demangle(typeid(T).name());
}

using cstring = std::string_view;

template <typename T>
concept uint_comparable_t = std::convertible_to<T, uint8_t>;

// Equality operator
template <uint_comparable_t T>
bool operator==(const T& one, const T& other) {
  return (uint64_t)one == (uint64_t)other;
}

// Less than operator
template <uint_comparable_t T>
bool operator<(const T& one, const T& other) {
  return (uint64_t)one < (uint64_t)other;
}

// Greater than operator
template <uint_comparable_t T>
bool operator>(const T& one, const T& other) {
  return (uint64_t)one > (uint64_t)other;
}

// Less than or equal to operator
template <uint_comparable_t T>
bool operator<=(const T& one, const T& other) {
  return !((uint64_t)one > (uint64_t)other);
}

// Greater than or equal to operator
template <uint_comparable_t T>
bool operator>=(const T& one, const T& other) {
  return !((uint64_t)one > (uint64_t)other);
}

// Not equal operator
template <uint_comparable_t T>
bool operator!=(const T& one, const T& other) {
  return !((uint64_t)one == (uint64_t)other);
}

template <typename T, size_t N>
class generator {
  size_t counter{};
  std::array<T, N> data;
  std::function<T(T)> before_each_fn;

public:
  generator(
      decltype(data) d,
      decltype(before_each_fn) fn = [](T t) { return t; })
      : data(d),
        before_each_fn(fn) {}
  T next() { return before_each_fn(data[counter++]); }
  void reset() { counter = 0; }
};

template <typename T, typename Fn = void> class property {
public:
  explicit property(const T& v)
      : value(v) {}
  explicit property(T&& v)
      : value(std::move(v)) {}

  property& operator=(const T& v) { value = v; }
  property& operator=(T&& v) { value = std::move(v); }
  property& operator=(const property& other) { this->value = other.value; }
  operator const T&() const { return value; }
  operator T() const { return value; }

private:
  property(const property&) = default;
  T value;
};

template <typename T>
class member_ptr {

public:
  member_ptr() = default;
  template <typename U>
    requires std::is_convertible_v<U*, T*>
  member_ptr(std::unique_ptr<U>&&);
  template <typename U>
    requires std::is_convertible_v<U*, T*>
  explicit member_ptr(U*);
  ~member_ptr();
  member_ptr(member_ptr&&)            = default;
  member_ptr& operator=(member_ptr&&) = default;
  T* get() const;
  T* operator->() const { return get(); }
  T& operator*() const { return *get(); }
  [[nodiscard]] bool owns() const;
  std::unique_ptr<T> release();
};

template <typename T>
concept ScopedEnum = requires { std::is_scoped_enum<T>(); };

template <ScopedEnum>
class enumeration;

template <typename E>
concept Enumerable = DerivedFromTemplate<E, enumeration> && ScopedEnum<E>;

template <ScopedEnum E>
struct enumeration : public formattable {
  constexpr enumeration();
  constexpr enumeration& operator=(const enumeration&) = default;
  constexpr enumeration& operator=(enumeration&&)      = default;
  constexpr enumeration(enumeration&&)                 = default;
  constexpr enumeration(const enumeration&)            = default;
  constexpr enumeration(E e)
      : m_value(e) {};
  constexpr ~enumeration() override = default;

  // Conversion
  constexpr operator E() const { return inner(); };
  constexpr operator uint8_t() const { return (uint8_t)value(); };
  template <typename To>
  [[nodiscard]] constexpr bool is_castable() const;
  template <typename To>
  constexpr To cast() const;

  // Data
  static constexpr auto type_name() noexcept;
  static constexpr auto count() noexcept { return magic_enum::enum_count<E>(); }
  [[nodiscard]] constexpr auto name() const noexcept;
  [[nodiscard]] constexpr E inner() const noexcept { return m_value; }
  [[nodiscard]] constexpr auto value() const noexcept;

  [[nodiscard]] constexpr std::string format() const override;

  using value_type = E;

protected:
  E m_value;
};

#define CREATE_ENUMERATION(TYPE, ...) \
  using value_type   = CONCAT(TYPE, _t); \
  using element_type = TYPE; \
  using enumeration<value_type>::enumeration; \
  using enum value_type; \
  DECLARE_VARS(__VA_ARGS__) \
  using member_types   = std::tuple<GET_TYPES(__VA_ARGS__)>; \
  using properties_map = magic_enum::containers::array<value_type, member_types>; \
  static_assert(std::is_constant_evaluated()); \
  [[nodiscard]] static constexpr const properties_map& properties_array(); \
  constexpr TYPE(value_type e) \
      : enumeration<value_type>(e), \
        CTOR_ASSIGN(__VA_ARGS__) {}

#define CREATE_ENUMERATION_CLASS(TYPE, ...) \
  struct TYPE : public cmm::enumeration<CONCAT(TYPE, _t)> { \
    CREATE_ENUMERATION(TYPE, __VA_ARGS__) \
  };

#define BUILD_ENUMERATION(TYPE, ...) \
  using value_type   = CONCAT(_, TYPE); \
  using element_type = TYPE; \
  using enumeration<value_type>::enumeration; \
  using enum value_type; \
  DECLARE_VARS(__VA_ARGS__) \
  using member_types   = std::tuple<GET_TYPES(__VA_ARGS__)>; \
  using properties_map = magic_enum::containers::array<value_type, member_types>; \
  static_assert(std::is_constant_evaluated()); \
  [[nodiscard]] static constexpr const properties_map& properties_array(); \
  constexpr TYPE(value_type e) \
      : enumeration<value_type>(e), \
        CTOR_ASSIGN(__VA_ARGS__) {}

#define BUILD_ENUMERATION_CLASS(TYPE, ...) \
  struct TYPE : public cmm::enumeration<CONCAT(_, TYPE)> { \
    BUILD_ENUMERATION(TYPE, __VA_ARGS__) \
  };

template <typename T, template <typename> class BaseTemplate>
concept DerivedFromTemplate = std::is_base_of_v<BaseTemplate<typename T::value_type>, T>;

template <typename T> class stack {
public:
  using value_type                   = T;
  using pointer_type                 = T*;
  using const_pointer_type           = const T*;
  using reference_type               = T&;
  using const_reference_type         = const T&;
  using iterator                     = std::vector<T>::reverse_iterator;
  using reverse_iterator             = std::vector<T>::iterator;
  using const_iterator               = std::vector<T>::const_reverse_iterator;
  using const_reverse_iterator       = std::vector<T>::const_iterator;

  stack()                            = default;
  ~stack()                           = default;
  stack(const stack&)                = default;
  stack(stack&&) noexcept            = default;
  stack& operator=(const stack&)     = default;
  stack& operator=(stack&&) noexcept = default;

  bool operator==(const stack& other) const noexcept;
  bool operator!=(const stack& other) const noexcept;

  iterator begin() noexcept;
  iterator end() noexcept;
  [[nodiscard]] const_iterator begin() const noexcept;
  [[nodiscard]] const_iterator end() const noexcept;
  [[nodiscard]] const_iterator cbegin() const noexcept;
  [[nodiscard]] const_iterator cend() const noexcept;
  [[nodiscard]] const_reverse_iterator crbegin() const noexcept;
  [[nodiscard]] const_reverse_iterator crend() const noexcept;

  T& top() noexcept;
  [[nodiscard]] const T& top() const noexcept;
  template <typename Func> bool contains(Func&& func) const;
  template <typename Func> const T& find(Func func) const;
  template <typename Func> std::optional<size_t> find_position(Func func) const;
  template <typename Func> const_iterator find_all(Func func) const;
  template <typename Func> size_t count(Func func) const;
  template <typename... Args> void emplace_back(Args&&... args)
    requires std::is_constructible_v<T, Args...>;
  void push(const T& t);
  void push(T&& t) noexcept;
  void pop();
  T&& pop_move();
  T pop_return();
  void clear() noexcept;
  void swap(stack& other) noexcept;
  [[nodiscard]] constexpr size_t size() const noexcept;
  [[nodiscard]] constexpr size_t max_size() const noexcept;
  [[nodiscard]] constexpr bool empty() const noexcept;

protected:
  std::vector<T> m_data;
};

// template <typename T>
// concept is_identifiable = std::is_base_of_v<identifiable<T>, T>;

// template <typename... Args>
// concept all_are_identifiable = (is_identifiable<Args> && ...);

template <typename Base, typename Derived>
struct is_direct_child_of {
  static constexpr bool value =
      std::is_base_of_v<Base, Derived> && !std::is_same_v<Base, Derived> &&
      !std::is_base_of_v<Base,
                         typename std::remove_pointer_t<decltype(static_cast<void*>(
                             static_cast<Base*>(static_cast<Derived*>(nullptr))))>>;
};

struct mangable {
  std::string mangled;
  virtual ~mangable() = default;
};

template <class T, class Tuple, size_t... Is>
T construct_from_tuple(Tuple&& tuple, std::index_sequence<Is...>) {
  return T{std::get<Is>(std::forward<Tuple>(tuple))...};
}

template <class T, class Tuple>
T construct_from_tuple(Tuple&& tuple) {
  return construct_from_tuple<T>(
      std::forward<Tuple>(tuple),
      std::make_index_sequence<std::tuple_size<std::decay_t<Tuple>>::value>{});
}

namespace adaptors {
  template <typename Adaptor, typename T, typename MemFn>
  auto make_pipe(const T& obj, MemFn mem_fn) {
    return Adaptor{[&obj, mem_fn](auto&& range) {
      return (obj.*mem_fn)(std::forward<decltype(range)>(range));
    }};
  }
  template <typename Func>
  struct process {
    Func func;

    template <typename Range, typename Alloc>
    constexpr auto operator()(Range&& range, Alloc& alloc) const {
      return func(std::forward<Range>(range), alloc);
    }
  };

  template <typename Range, template <typename> class Adaptor, typename F>
  auto operator|(Range&& range, const Adaptor<F>& adaptor) {
    return adaptor(std::forward<Range>(range));
  }

}; // namespace adaptors

template <typename T>
using ref = std::reference_wrapper<T>;
template <typename T>
using cref = std::reference_wrapper<const T>;

template <typename... Ts>
struct overloaded : Ts... {
  using Ts::operator()...;
};
template <typename... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

struct location : public formattable {
  uint32_t start{}, end{};

  constexpr location() = default;
  constexpr location(uint32_t start, uint32_t offset)
      : start(start),
        end(offset) {}

  [[nodiscard]] std::string format() const override;
};

static bool operator==(const location& r, const location& l) {
  return r.start == l.start && r.end == l.end;
}
static location operator+(const location& r, const location& l) {
  return {std::min(r.start, l.start), std::max(r.end, l.end)};
}

} // namespace cmm

template <>
struct std::formatter<std::optional<cmm::location>> : formatter<string_view> {
  auto format(const std::optional<cmm::location>& loc, format_context& ctx) const {
    if (loc) {
      return formatter<string_view>::format(std::format("{}", loc.value()), ctx);
    }
    return formatter<string_view>::format("<No location>", ctx);
  }
};
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

  [[nodiscard]] constexpr const char* what() const noexcept override { return message.data(); }

protected:
  std::string message;
};

template <typename Base, typename Derived>
bool check_type(Base* ptr) {
  return dynamic_cast<Derived*>(ptr);
}

enum class compilation_error_t : uint8_t {
  GENERIC,
  INVALID_CONTINUE,
  INVALID_BREAK,
  UNDECLARED_SYMBOL,
  ALREADY_DECLARED_SYMBOL,
  UNDEFINED_FUNCTION,
  LABEL_IN_GLOBAL,
  RETURN_IN_GLOBAL,
  BAD_FUNCTION_CALL,
  WRONG_FUNCTION_ARGUMENT,
  UNEXPECTED_TOKEN,
  INCOMPATIBLE_TOKEN,
  REQUIRED_TYPE,
  TOO_MANY_TYPES,
  MISSING_ENTRY_POINT
};

BUILD_ENUMERATION_DATA_CLASS(compilation_error, os::status, status, cstring, fmt, bool, located);

struct compilation_error : public cmm::error {
  cmm::os::status status;
  std::optional<cmm::location> loc;

  compilation_error(const compilation_error_data& err,
                    const std::string& str,
                    std::optional<location> loc = {})
      : error(str),
        status(err.status),
        loc(std::move(loc)) {}
};

template <typename T>
concept is_allocated = requires(T t) {
  { t.location() } -> std::same_as<cmm::location>;
};

template <compilation_error_t Err, typename T>
[[noreturn]] void throw_error(T&& t) {
  constexpr auto err = compilation_error_data(Err);
  throw compilation_error(err, std::format(err.fmt, t));
}

template <compilation_error_t Err, typename L>
  requires(is_allocated<L>)
[[noreturn]] void throw_error(L&& l) {
  constexpr auto err = compilation_error_data(Err);
  auto msg           = std::format(err.fmt, l);
  throw compilation_error(err, msg, l.location());
}

template <typename T>
struct default_singleton {
  NOT_COPYABLE_CLS(default_singleton);
  static T& instance() {
    static T instance;
    return instance;
  }

  friend T;

protected:
  default_singleton()  = default;
  ~default_singleton() = default;
};

template <typename T>
class singleton {
public:
  // Get instance with initialization parameters (only works on first call)
  template <typename... Args>
  static T& instance(Args&&... args) {
    std::call_once(initialized_flag,
                   [&]() { instance_ptr = std::make_unique<T>(std::forward<Args>(args)...); });
    return *instance_ptr;
  }

  // Get instance without parameters (throws if not initialized)
  static T& instance() {
    if (!instance_ptr) {
      throw std::runtime_error("singleton not initialized. Call instance() "
                               "with parameters first.");
    }
    return *instance_ptr;
  }

  // Check if singleton is initialized
  static bool is_initialized() { return instance_ptr != nullptr; }

  // Prevent copying and assignment
  NOT_MOVABLE_CLS(singleton);
  NOT_COPYABLE_CLS(singleton);

protected:
  singleton()          = default;
  virtual ~singleton() = default;

  // Allow derived class to access the constructor
  friend T;

private:
  static std::unique_ptr<T> instance_ptr;
  static std::once_flag initialized_flag;
};

template <typename T>
std::unique_ptr<T> singleton<T>::instance_ptr = nullptr;

template <typename T>
std::once_flag singleton<T>::initialized_flag;

inline uint8_t string_hash(const std::string& str) {
  unsigned long hash = 0;
  for (char c : str) {
    hash = hash * 31 + static_cast<unsigned char>(c); // Multiply by 31 and add the character
  }
  return hash;
}

struct config {
  bool dump_tokens;
  bool dump_ast;
  bool dump_state;
  bool dump_memory;
  constexpr static bool preprocessed = SAVE_PREPROCESSED;
  constexpr static bool assembly     = SAVE_ASSEMBLY;

  config(bool dump_tokens, bool dump_ast, bool dump_state, bool dump_memory)
      : dump_tokens(dump_tokens),
        dump_ast(dump_ast),
        dump_state(dump_state),
        dump_memory(dump_memory) {}
};

class source_code {
public:
  source_code(const fs::ifile&);

  [[nodiscard]] const std::string& get_code() const;
  [[nodiscard]] const std::string& get_filename() const;
  [[nodiscard]] bool is_valid(const location&) const;
  [[nodiscard]] std::pair<size_t, size_t> get_line(const location&) const;
  [[nodiscard]] std::string get_chunk(const location&) const;
  [[nodiscard]] std::tuple<std::string, std::string, std::string> get_line_chunked(
      const location&) const;

private:
  std::string m_code;
  std::string m_filename;
};

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
  [[nodiscard]] cstring snapshot() const noexcept;

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

// // Store by value
// template <class T>
// struct ByValue {
//   using stored_type = T;
//   static T& get(stored_type& v) { return v; }
//   static const T& get(const stored_type& v) { return v; }
// };
//
// // Store by unique_ptr
// template <class T>
// struct ByUniquePtr {
//   using stored_type = std::unique_ptr<T>;
//   static T& get(stored_type& p) { return *p; }
//   static const T& get(const stored_type& p) { return *p; }
// };
//
// // Store by raw pointer
// template <class T>
// struct ByRawPtr {
//   using stored_type = T*;
//   static T& get(stored_type p) { return *p; }
//   static const T& get(const stored_type p) { return *p; }
// };
//
// // Store by reference_wrapper
// template <class T>
// struct ByRefWrap {
//   using stored_type = std::reference_wrapper<T>;
//   static T& get(stored_type w) { return w.get(); }
//   static const T& get(const stored_type w) { return w.get(); }
// };
// template <typename T>
// struct select_storage_policy {
//   using type = ByValue<T>;
// };
//
// template <typename T>
// struct select_storage_policy<std::unique_ptr<T>> {
//   using type = ByUniquePtr<T>;
// };
//
// template <typename T>
// struct select_storage_policy<T*> {
//   using type = ByRawPtr<T>;
// };
//
// template <typename T>
// struct select_storage_policy<std::reference_wrapper<T>> {
//   using type = ByRefWrap<T>;
// };
template <class T>
class vector {
public:
  using value_type             = T;
  using element_type           = T;
  using pointer_type           = std::add_pointer_t<value_type>;
  using const_pointer_type     = const pointer_type;
  using reference_type         = std::add_lvalue_reference_t<value_type>;
  using const_reference_type   = const reference_type;
  using rvalue_type            = std::add_rvalue_reference_t<value_type>;
  using container_type         = std::vector<element_type, std::allocator<element_type>>;
  using iterator               = typename container_type::iterator;
  using const_iterator         = typename container_type::const_iterator;
  using reverse_iterator       = typename container_type::reverse_iterator;
  using const_reverse_iterator = typename container_type::const_reverse_iterator;

  vector()                     = default;
  vector(std::initializer_list<value_type> init);
  vector(const container_type&);
  vector(container_type&&);
  virtual ~vector() = default;
  operator container_type() const { return m_data; }
  operator const container_type&() const { return m_data; }
  [[nodiscard]] const container_type& data() const;
  T& at(size_t);
  [[nodiscard]] const T& at(size_t) const;
  reference_type front();
  [[nodiscard]] const_reference_type front() const;
  reference_type back();
  [[nodiscard]] const_reference_type back() const;
  iterator begin();
  iterator end();
  [[nodiscard]] const_iterator begin() const;
  [[nodiscard]] const_iterator end() const;
  [[nodiscard]] const_iterator cbegin() const;
  [[nodiscard]] const_iterator cend() const;
  reverse_iterator rbegin();
  reverse_iterator rend();
  [[nodiscard]] const_reverse_iterator rbegin() const;
  [[nodiscard]] const_reverse_iterator rend() const;
  [[nodiscard]] bool empty() const;
  [[nodiscard]] size_t size() const;
  void push_back(const T&);
  void push_back(T&&);
  template <typename Fn>
  pointer_type find(Fn);
  template <typename Fn>
  const_pointer_type find(Fn) const;
  template <typename Fn>
  auto transform(Fn&&) const;
  [[nodiscard]] std::string join(char, size_t) const;
  template <std::ranges::forward_range Pattern>
    requires(std::ranges::view<Pattern>)
  std::string join(Pattern&&, size_t) const;
  template <std::move_constructible Func, std::ranges::forward_range Pattern>
    requires(std::ranges::view<Pattern>)
  std::string join(Func&&, Pattern&&, size_t) const;

private:
  container_type m_data;
};

template <typename K, typename V>
class hashmap {
public:
  using key_type       = K;
  using value_type     = V;
  using container_type = std::unordered_map<std::string, value_type>;
  hashmap();
  const container_type& data() const { return m_store; }
  [[nodiscard]] bool contains(const key_type&) const;
  [[nodiscard]] size_t size() const noexcept;
  container_type::const_iterator begin() const { return m_store.begin(); }
  container_type::const_iterator end() const { return m_store.end(); }
  container_type::const_iterator cbegin() const { return m_store.begin(); }
  container_type::const_iterator cend() const { return m_store.cend(); }
  value_type& at(const key_type&);
  const value_type& at(const key_type&) const;
  void insert(const key_type&, value_type&&);
  void insert(const key_type&, const value_type&);
  value_type& operator[](const key_type&);
  const value_type& operator[](const key_type&) const;
  template <typename... Args>
    requires std::is_constructible_v<V, Args...>
  value_type& emplace(key_type, Args&&...);
  void clear();

private:
  container_type m_store;
};
static_assert(std::is_copy_constructible_v<vector<int>>);

template <typename T>
struct node {
  node* next;
  T value;
};

namespace traits {

  template <typename T, T V>
  struct integral_constant {
    static constexpr T value = V;
    using value_type         = T;
    using type               = integral_constant<T, V>;
    constexpr operator value_type() const noexcept { return value; }
    constexpr value_type operator()() const noexcept { return value; }
  };
  template <bool B>
  using bool_constant = integral_constant<bool, B>;
  using true_type     = bool_constant<true>;
  using false_type    = bool_constant<false>;

}; // namespace traits

template <typename Range, typename Func>
auto forward_range(Range&& range, Func&& fn) {
  auto make_ref_tuple = [&range]<std::size_t... Indices>(std::index_sequence<Indices...>) {
    return std::tie(range[Indices]...);
  };

  constexpr auto size = std::tuple_size_v<std::remove_reference_t<Range>>;
  auto ref_tuple      = make_ref_tuple(std::make_index_sequence<size>{});
  return std::apply([fn](auto&&... args) { return fn(args...); }, ref_tuple);
}

#define transform_vector(IN, FN) IN | std::views::transform(FN) | std::ranges::to<std::vector>()

template <typename T, typename... Args>
concept is_constructible = requires(Args&&... args) {
  { T{std::forward<Args>(args)...} } -> std::same_as<T>;
  !std::is_abstract_v<T>;
};

template <typename T>
constexpr inline auto generate = [](T& t) { return t; };

#define GET_MEMBER(OBJ, MEMBER) \
  if constexpr (requires { OBJ.operator->(); }) { \
    OBJ->MEMBER \
  } else if constexpr (std::is_pointer_v<decltype(OBJ)>) { \
    OBJ->MEMBER \
  } else { \
    OBJ.MEMBER \
  }

template <typename... Ts> // (7)
struct overload : Ts... {
  using Ts::operator()...;
};
template <class... Ts> overload(Ts...) -> overload<Ts...>;

template <typename Key, typename Value, std::size_t Size>
struct constexpr_map {
  using container_type  = std::array<std::pair<Key, Value>, Size>;
  using key_type        = Key;
  using mapped_type     = Value;
  using value_type      = typename container_type::value_type;
  using size_type       = typename container_type::size_type;
  using difference_type = typename container_type::difference_type;
  using const_reference = typename container_type::const_reference;
  using reference       = const_reference;
  using const_pointer   = typename container_type::const_pointer;
  using pointer         = const_pointer;
  using const_iterator  = const_pointer;
  using iterator        = const_iterator;

  constexpr constexpr_map()
      : m_data() {}
  constexpr constexpr_map(container_type items)
      : m_data(items) {}
  // Constructor from initializer list (C++14 and later)
  constexpr constexpr_map(std::initializer_list<value_type> items)
      : m_data{} {
    std::size_t i = 0;
    for (const auto& item : items) {
      if (i >= Size) {
        break; // Safety check
      }
      m_data[i++] = item;
    }
  }
  // Constructor from array
  template <std::size_t N>
  constexpr constexpr_map(const std::pair<Key, Value> (&items)[N])
      : m_data() {
    static_assert(N <= Size, "Too many items for constexpr_map");
    for (std::size_t i = 0; i < N; ++i) {
      m_data[i] = items[i];
    }
  }

  [[nodiscard]] constexpr const Value& at(const Key& key) const {
    const auto itr = std::find_if(
        m_data.begin(), m_data.end(), [&key](const auto& v) { return v.first == key; });
    if (itr != m_data.end()) {
      return itr->second;
    }

    throw std::range_error(std::format("Not found {}", key));
  }

private:
  std::array<std::pair<Key, Value>, Size> m_data;
};

template <typename T, typename V, std::size_t N>
constexpr auto make_constexpr_map(std::pair<T, V> const (&items)[N]) {
  return constexpr_map<T, V, N>{items};
}
} // namespace cmm

#include "common.inl"
