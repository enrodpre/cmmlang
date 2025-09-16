#pragma once

#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cxxabi.h>
#include <exception>
#include <initializer_list>
#include <magic_enum/magic_enum_all.hpp>
#include <stdexcept>
#include <type_traits>

#include "logging.hpp"
#include "macros.hpp"
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

template <class Range, class T>
concept is_range_asignable = std::ranges::input_range<Range> &&
                             std::convertible_to<std::ranges::range_reference_t<Range>, T>;

template <typename T>
class stack {
public:
  using value_type                   = T;
  using pointer_type                 = T*;
  using container_type               = std::vector<T>;
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

  stack(const container_type&);
  stack(container_type&&);
  template <is_range_asignable<T> Range>
  stack(Range&&);

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
  template <typename Func>
  bool contains(Func&& func) const;
  template <typename Func>
  const T& find(Func func) const;
  template <typename Func>
  std::optional<size_t> find_position(Func func) const;
  template <typename Func>
  const_iterator find_all(Func func) const;
  template <typename Func>
  size_t count(Func func) const;
  template <typename... Args>
    requires std::is_constructible_v<T, Args...>
  void emplace(Args&&... args);
  void push(const T& t);
  void push(T&& t) noexcept;
  void pop();
  auto pop_value();
  void clear() noexcept;
  void swap(stack& other) noexcept;
  [[nodiscard]] constexpr size_t size() const noexcept;
  [[nodiscard]] constexpr size_t max_size() const noexcept;
  [[nodiscard]] constexpr bool empty() const noexcept;

protected:
  container_type m_data;
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

  compilation_error(const compilation_error_data& err,
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

template <typename T>
struct default_singleton {
  friend T;
  NOT_COPYABLE_CLS(default_singleton);

  static T& instance() {
    static T instance;
    return instance;
  }

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

// template <class T, class Alloc = std::allocator<T>>
// class vector {
// public:
//   using value_type             = T;
//   using element_type           = T;
//   using allocator_type         = Alloc;
//   using pointer_type           = std::add_pointer_t<value_type>;
//   using const_pointer_type     = const pointer_type;
//   using reference_type         = std::add_lvalue_reference_t<value_type>;
//   using const_reference_type   = const reference_type;
//   using rvalue_type            = std::add_rvalue_reference_t<value_type>;
//   using container_type         = std::vector<element_type, Alloc>;
//   using iterator               = typename container_type::iterator;
//   using const_iterator         = typename container_type::const_iterator;
//   using reverse_iterator       = typename container_type::reverse_iterator;
//   using const_reverse_iterator = typename container_type::const_reverse_iterator;
//
//   vector()                     = default;
//   vector(std::initializer_list<value_type> init);
//   vector(const container_type&);
//   vector(container_type&&);
//   virtual ~vector() = default;
//
//   operator container_type() const { return m_data; }
//   operator const container_type&() const { return m_data; }
//
//   [[nodiscard]] const container_type& data() const;
//   T& at(size_t);
//   [[nodiscard]] const T& at(size_t) const;
//   reference_type front();
//   [[nodiscard]] const_reference_type front() const;
//   reference_type back();
//   [[nodiscard]] const_reference_type back() const;
//   iterator begin();
//   iterator end();
//   [[nodiscard]] const_iterator begin() const;
//   [[nodiscard]] const_iterator end() const;
//   [[nodiscard]] const_iterator cbegin() const;
//   [[nodiscard]] const_iterator cend() const;
//   reverse_iterator rbegin();
//   reverse_iterator rend();
//   [[nodiscard]] const_reverse_iterator rbegin() const;
//   [[nodiscard]] const_reverse_iterator rend() const;
//   [[nodiscard]] bool empty() const;
//   [[nodiscard]] size_t size() const;
//   void push_back(const T&);
//   void push_back(T&&);
//   template <typename Fn>
//   pointer_type find(Fn);
//   template <typename Fn>
//   const_pointer_type find(Fn) const;
//   template <typename Fn>
//   auto transform(Fn&&) const;
//   [[nodiscard]] std::string join(char, size_t) const;
//   template <std::ranges::forward_range Pattern>
//     requires(std::ranges::view<Pattern>)
//   std::string join(Pattern&&, size_t) const;
//   template <std::move_constructible Func, std::ranges::forward_range Pattern>
//     requires(std::ranges::view<Pattern>)
//   std::string join(Func&&, Pattern&&, size_t) const;
//
// private:
//   container_type m_data;
// };

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
constexpr auto add_constexpr_map(std::pair<T, V> const (&items)[N]) {
  return constexpr_map<T, V, N>{items};
}
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
  return magic_enum::enum_flags_contains<value_category_t>(lhs & rhs);
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

template <typename T,
          typename D,
          std::enable_if_t<std::is_standard_layout_v<T> && std::is_trivially_copyable_v<T>, int> =
              0,
          std::enable_if_t<std::is_nothrow_move_constructible_v<T>, int> = 0>
class raii_wrapper {
  T obj;
  std::optional<D> deleter;

public:
  raii_wrapper(T obj, D deleter)
      : obj(obj),
        deleter(deleter) {}
  raii_wrapper(raii_wrapper&& other) noexcept
      : obj(std::move(other.obj)),
        deleter(std::move(other.deleter)) {
    other.deleter = std::nullopt;
  }
  raii_wrapper(const raii_wrapper&)            = delete;
  raii_wrapper& operator=(raii_wrapper&&)      = delete;
  raii_wrapper& operator=(const raii_wrapper&) = delete;
  ~raii_wrapper() {
    if (deleter.has_value()) {
      auto t = deleter.value();
      t(obj);
    }
  }
  operator T&() { return obj; }
  operator const T&() const { return obj; }
  T& get() { return obj; }
  [[nodiscard]] const T& get() const { return obj; }
};
template <typename T, typename D>
raii_wrapper<std::remove_reference_t<T>, D> raii_wrap(T obj, D deleter) {
  return raii_wrapper<std::remove_reference_t<T>, D>(obj, deleter);
}

inline std::string demangle(const std::string& name, bool check_prefix = true) {
  if (check_prefix && !(name.starts_with("_Z") || name.starts_with("__Z"))) {
    return name;
  }
  std::size_t offset = 0;
  if (name.starts_with("__Z")) {
    offset = 1;
  }
  auto end = name.find(' ');
  std::string name_copy;
  std::reference_wrapper<const std::string> to_demangle = name;
  std::string rest;
  if (end != std::string::npos) {
    name_copy   = name.substr(0, end);
    rest        = name.substr(end);
    to_demangle = name_copy;
  }
  int status = 0;
  auto demangled =
      raii_wrap(abi::__cxa_demangle(to_demangle.get().c_str() + offset, nullptr, nullptr, &status),
                [](char* str) { std::free(str); });
  if (demangled.get() != nullptr) {
    std::string str = demangled.get();
    if (!rest.empty()) {
      str += rest;
    }
    return str;
  }
  return name;
}
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

template <typename Args>
std::string mangle_function(std::string id, Args args, char delim) {
  auto format_args = args | std::views::join_with(delim) | std::ranges::to<std::string>();
  return std::format("{}{}", id, !format_args.empty() ? std::format("_{}", format_args) : "");
}

} // namespace cmm

#include "common.inl"
