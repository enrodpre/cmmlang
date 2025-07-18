#pragma once

#include "common.hpp"
#include "fs.hpp"
#include "stl.hpp"
#include <cstdint>
#include <regex>

namespace cmm {
using cstring = std::string_view;
};

namespace cmm::strings {

using svmatch     = std::match_results<cstring::const_iterator>;
using svsub_match = std::sub_match<cstring::const_iterator>;

enum class _style_t : uint8_t {
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
BUILD_ENUMERATION_CLASS(style_t, _style_t, value, std::string_view, pre);

template <typename T, char Delim>
std::string join();
struct colorizer {
  STATIC_CLS(colorizer);
  constexpr static std::string_view RESET_ANSI = "\033[0m";

  template <_style_t S, typename T>
    requires(std::formattable<T, char>)
  constexpr static std::string colorize(T&&);
  template <typename T>
  constexpr static std::string header_style(T&&);
  template <typename T>
  constexpr static std::string bold_style(T&&);
  template <typename T>
  constexpr static std::string error_style(T&&);
};

size_t find_next_newline(cstring);
bool match_regex_sv(cstring, cstring, svmatch&);

template <typename T>
std::string classname_only();

class source_code {
public:
  source_code(const fs::ifile&);

  [[nodiscard]] const std::string& get_code() const;
  [[nodiscard]] const std::string& get_filename() const;
  [[nodiscard]] bool is_valid(const location&) const;
  [[nodiscard]] std::string get_line(size_t) const;
  [[nodiscard]] std::string get_chunk(const location&) const;
  [[nodiscard]] std::tuple<std::string, std::string, std::string>
  get_line_chunked(const location&) const;

private:
  std::string m_code;
  std::string m_filename;
};

[[nodiscard]] std::string generate_compilation_message(const source_code& src,
                                                       cstring,
                                                       const location&);

inline std::string highlight(const source_code& code, const location& range) {
  auto [left, error, right] = code.get_line_chunked(range);
  auto formatted_error      = colorizer::error_style(error);
  return std::format("{}{}{}\n", left, formatted_error, right);
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

  string_buffer() { m_actives.emplace_back(); }
  string_buffer& operator<<(string_buffer&);
  string_buffer& operator<<(const std::string&);

  // Main generators
  template <size_t, typename... Args>
  constexpr string_buffer& write(std::format_string<Args...>,
                                 Args&&...) noexcept;
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
constexpr string_buffer& string_buffer::write(
    std::format_string<Args...> std_string,
    Args&&... args) noexcept {
  if constexpr (IndentLvl > 0) {
    active() << std::format("{:{}}", "", IndentLvl);
  }
  active() << std::format(std_string, std::forward<Args>(args)...);
  return *this;
}

template <size_t Times = 1>
constexpr string_buffer& strings::string_buffer::newline() noexcept {
  active() << '\n';
  if constexpr (Times > 1) {
    return newline<Times - 1>();
  } else {
    return *this;
  }
}

class string {
  std::string string_;

public:
  string(std::string&&);
  [[nodiscard]] cstring get_line(size_t line_n) const;
};
} // namespace cmm::strings

#include "strings.inl"
