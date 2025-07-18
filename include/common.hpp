#pragma once

#include "os.hpp"
#include "spdlog/spdlog.h"
#include "traits.hpp"
#include <bits/version.h>
#include <exception>
#include <format>
#include <functional>
#include <libassert/assert.hpp>
#include <magic_enum/magic_enum_all.hpp>
#include <memory>
#include <mutex>
#include <print>
#include <ranges>
#include <string>
#include <sys/types.h>
#include <tuple>
#include <type_traits>
#include <typeindex>
#include <utility>
#include <utils.hpp>

#define CMM_UNREACHABLE(stdstr, ...) \
  UNREACHABLE(std::format(stdstr, ##__VA_ARGS__))
#ifndef SAVE_PREPROCESSED
  #define SAVE_PREPROCESSED 0
#endif
#ifndef SAVE_ASSEMBLY
  #define SAVE_ASSEMBLY 0
#endif

#define FORMAT_DECL_IMPL() std::string format() const override;
#define FORMAT_IMPL(TYPE, stdstr, ...) \
  std::string TYPE::format() const { \
    return std::format(stdstr, __VA_ARGS__); \
  }

#define MOVABLE_CLS(CLS) \
  CLS(CLS&&)            = default; \
  CLS& operator=(CLS&&) = default;

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

namespace cmm {

constexpr static uint8_t DATASIZE      = 8;
constexpr static uint8_t MAX_ARGUMENTS = 6;
constexpr static uint8_t WORD_LEN      = DATASIZE; // bytes
constexpr static uint8_t MEM_ADDR_LEN  = WORD_LEN; // bytes

using ushort                           = unsigned short;
using cstring                          = std::string_view;

struct hashable {};

struct formattable {
  constexpr virtual ~formattable()                           = default;
  [[nodiscard]] constexpr virtual std::string format() const = 0;
};

template <std::ranges::range T>
struct formattable_range {
  using value_type = std::ranges::range_value_t<T>;

  formattable_range(T*);
  static_assert(std::formattable<std::ranges::range_value_t<T>, char>);

  template <class Delim>
  [[nodiscard]] constexpr std::string join(Delim&&) const;
  auto element_merger() const;

private:
  T* m_range;
};

} // namespace cmm

// Specialization for cmm::formattable itself
template <typename T>
struct std::
    formatter<T, std::enable_if_t<std::is_base_of_v<cmm::formattable, T>, char>>
    : std::formatter<string_view> {

  template <typename Ctx>
  auto format(const T& obj,
              Ctx& ctx) const { // Marked as const
    return std::formatter<string_view>::format(obj.format(), ctx);
  }
};

template <typename T>
struct std::formatter<
    T*,
    std::enable_if_t<std::is_base_of_v<cmm::formattable, T>, char>>
    : std::formatter<T> {

  template <typename Ctx>
  auto format(const T* formattablePtr, Ctx& ctx) const {
    if (formattablePtr) {
      return std::formatter<string_view>::format(formattablePtr->format(), ctx);
    }
    return std::formatter<string_view>::format("nullptr", ctx);
  }
};

template <typename T>
concept is_smart_pointer_formattable =
    std::is_base_of_v<cmm::formattable, typename T::element_type> &&
    (std::is_same_v<T, std::unique_ptr<typename T::element_type>> ||
     std::is_same_v<
         T,
         std::shared_ptr<typename T::element_type>>); // Specialization for
                                                      // pointers to subclasses
                                                      // of cmm::formattable
template <IsSmartOrRawPointer T>
struct std::formatter<T, char> : std::formatter<string_view> {

  template <typename Ctx>
  auto format(const T& formattablePtr, Ctx& ctx) const {
    if (formattablePtr) {
      return std::formatter<string_view>::format(formattablePtr->format(), ctx);
    }
    return std::formatter<string_view>::format("nullptr", ctx);
  }
};

template <typename T>
struct std::formatter<
    T,
    std::enable_if_t<
        std::is_same_v<T, std::unique_ptr<typename T::element_type>>,
        char>> : std::formatter<string_view> {

  template <typename Ctx>
  auto format(const std::unique_ptr<typename T::element_type>& formattablePtr,
              Ctx& ctx) const {
    if (formattablePtr) {
      return std::formatter<string_view>::format(formattablePtr->format(), ctx);
    }
    return std::formatter<string_view>::format("nullptr", ctx);
  }
};

namespace cmm {

// template <typename T>
// concept is_identifiable = std::is_base_of_v<identifiable<T>, T>;

// template <typename... Args>
// concept all_are_identifiable = (is_identifiable<Args> && ...);
template <typename Cls, typename Parent, typename... Parents>
struct identifiable;

template <typename Base, typename Derived>
struct is_direct_child_of {
  static constexpr bool value =
      std::is_base_of_v<Base, Derived> && !std::is_same_v<Base, Derived> &&
      !std::is_base_of_v<
          Base,
          typename std::remove_pointer_t<decltype(static_cast<void*>(
              static_cast<Base*>(static_cast<Derived*>(nullptr))))>>;
};

template <typename Cls, typename Parent = void, typename... Parents>
struct identifiable {
  using id_type = size_t;
  identifiable();
  virtual ~identifiable();

  [[nodiscard]] virtual std::string repr() const;
  [[nodiscard]] virtual std::string id() const;
  [[nodiscard]] size_t id_n() const;

  using return_type =
      std::conditional_t<std::is_void_v<Parent>, std::nullptr_t, const Parent*>;

  virtual return_type parent() const = 0;

  static id_type constructed();
  static id_type destructed();
  static id_type active();

private:
  id_type m_id;
  static inline id_type m_constructions = 0;
  static inline id_type m_destructions  = 0;
};

template <typename T, typename... Ts>
struct identifiable_child : public identifiable<T, Ts...> {
  using identifiable<T, Ts...>::identifiable;
};

template <typename T>
struct identifiable_parent : public identifiable<T> {
  using identifiable<T>::identifiable;
  [[nodiscard]] std::nullptr_t parent() const override;
  // std::nullptr_t parent() override { return nullptr; }
};
template <typename T>
inline std::nullptr_t identifiable_parent<T>::parent() const {
  return nullptr;
}

template <typename T>
concept Formattable = std::is_base_of_v<formattable, T>;

struct mangable {
  std::string mangled;
  virtual ~mangable() = default;
};

template <typename, typename = void>
struct has_tie_function : std::false_type {};
template <typename T>
struct has_tie_function<T, std::void_t<decltype(std::declval<T>().tie())>>
    : std::true_type {};
template <typename T>
concept HashableForTied =
    std::is_base_of_v<hashable, T> && has_tie_function<T>::value;

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
}; // namespace cmm

template <cmm::HashableForTied T>
struct std::hash<T> {
private:
  std::size_t combine_hash(std::size_t hash1, std::size_t hash2) noexcept {
    return hash1 ^ (hash2 + 0x9e3779b9 + (hash1 << 6) +
                    (hash1 >> 2)); // Boost's hash combine
  }
  template <typename... Ts>
  auto combined_hash(const Ts&... ts) {
    size_t seed{};
    (..., combine_hash(seed, std::hash<Ts>{}(ts)));
    return seed;
  }

public:
  size_t operator()(const T& v) const {
    auto hasher = [this](const auto&... args) {
      return combined_hash(std::type_index(typeid(T)), args...);
    };

    return std::apply(hasher, v.tie());
  }
  template <typename... Args>
  size_t operator()(bool c, bool v, Args&&... args) {
    return combined_hash(std::type_index(typeid(T)), c, v, args...);
  }
};

namespace cmm {
template <typename T>
using ref = std::reference_wrapper<T>;
template <typename T>
using cref = std::reference_wrapper<const T>;

template <typename T>
using ptr = std::shared_ptr<T>;

template <typename... Ts>
struct overloaded : Ts... {
  using Ts::operator()...;
};
template <typename... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

struct range : public formattable {
  size_t start, length;
  constexpr range()
      : start(0),
        length(0) {}
  constexpr range(size_t start_, size_t len)
      : start(start_),
        length(len) {}
  [[nodiscard]] constexpr auto tie() const { return std::tie(start, length); }

  constexpr range operator+(const range& other) const {
    size_t start = std::min(start, other.start);
    size_t end = std::max(start, other.start) + std::max(length, other.length);
    size_t length = end - start;

    return {start, length};
  }
  [[nodiscard]] std::string format() const override {
    if (length != 0) {
      return std::format("{}:{}", start, start + length);
    }

    return std::format("{}", start);
  }
};

static_assert(std::formattable<std::unique_ptr<range>, char>);
static_assert(std::formattable<std::shared_ptr<range>, char>);
static_assert(std::formattable<range*, char>);

static bool operator==(const range& r, const range& l) {
  return r.start == l.length && l.start == r.length;
}

struct location : public formattable {
  range rows, cols;

  constexpr location() = default;
  constexpr location(size_t row, size_t row_len, size_t col, size_t col_length);
  constexpr location(range a, range b);

  constexpr location operator+(const location& right) const;
  [[nodiscard]] std::string format() const override;
};

static bool operator==(const location& r, const location& l) {
  return r.rows == l.rows && l.cols == r.cols;
}

#define GET_LOC(p) (p == nullptr ? location() : p->loc)

struct error : public std::exception {
  error()           = delete;
  ~error() override = default;
  error(std::string_view msg)
      : message(msg) {}

  [[nodiscard]] constexpr const char* what() const noexcept override {
    return message.data();
  }

protected:
  std::string message;
};

template <typename Base, typename Derived>
bool check_type(Base* ptr) {
  return dynamic_cast<Derived*>(ptr);
}

struct compilation_error : public cmm::error {
  cmm::os::status status;
  const location loc;

  compilation_error(cmm::os::status status,
                    location loc,
                    const std::string& msg)
      : error(std::move(msg)),
        status(status),
        loc(std::move(loc)) {
    std::print("{}", libassert::stacktrace());
  }
};

#define CREATE_ERROR(NAME, STATUS, FMTSTR) \
  struct NAME : public compilation_error { \
    template <typename... Args> \
    NAME(cmm::location loc, Args&&... args) \
        : compilation_error( \
              STATUS, \
              loc, \
              std::format(FMTSTR, std::forward<Args>(args)...)) {} \
  }

CREATE_ERROR(generic_error, os::status::GENERIC_ERROR, "Generic error");
CREATE_ERROR(invalid_continue,
             os::status::INVALID_CONTINUE,
             "continue statement not within loop or switch");
CREATE_ERROR(invalid_break,
             os::status::INVALID_BREAK,
             "break statement not within loop or switch");
CREATE_ERROR(undeclared_symbol,
             os::status::UNDECLARED_SYMBOL,
             "{} not declared");
CREATE_ERROR(already_declared_symbol,
             os::status::ALREADY_DECLARED_SYMBOL,
             "{} already declared");
CREATE_ERROR(label_in_global,
             os::status::LABEL_IN_GLOBAL,
             "Label {} in global scope");
CREATE_ERROR(return_in_global,
             os::status::RETURN_IN_GLOBAL,
             "Return in global scope");
CREATE_ERROR(bad_function_call,
             os::status::BAD_FUNCTION_CALL,
             "Label {} in global scope");
CREATE_ERROR(wrong_function_argument,
             os::status::WRONG_FUNCTION_ARGUMENT,
             "Wrong argument. Declared {} but provided {}");
CREATE_ERROR(unexpected_token,
             os::status::UNEXPECTED_TOKEN,
             "Unexpected token {}");
CREATE_ERROR(incompatible_token,
             os::status::INCOMPATIBLE_TOKEN,
             "Incompatible token {}. Already declared {}");
CREATE_ERROR(required_type,
             os::status::REQUIRED_TYPE,
             "Required type in specifiers");
CREATE_ERROR(too_many_types,
             os::status::UNEXPECTED_TOKEN,
             "More than one type in specifiers");
CREATE_ERROR(missing_entry_point,
             os::status::MISSING_ENTRY_POINT,
             "Main function not found");

template <typename T>
struct default_singleton {
  NOT_COPYABLE_CLS(default_singleton);
  NOT_MOVABLE_CLS(default_singleton);
  static T& instance() {
    static T instance;
    return instance;
  }

protected:
  default_singleton()  = default;
  ~default_singleton() = default;

  friend T;
};

template <typename T>
class singleton {
public:
  // Get instance with initialization parameters (only works on first call)
  template <typename... Args>
  static T& instance(Args&&... args) {
    std::call_once(initialized_flag, [&]() {
      instance_ptr = std::make_unique<T>(std::forward<Args>(args)...);
    });
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
    hash = hash * 31 + static_cast<unsigned char>(
                           c); // Multiply by 31 and add the character
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

} // namespace cmm

#include "common.inl"
