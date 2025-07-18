#pragma once

#include "strings.hpp"

namespace cmm::strings {

[[nodiscard]] constexpr const style_t::properties_map&
style_t::properties_array() {
  using enum _style_t;
  static constexpr properties_map MAP{{{{HEADER, "\033[40;1;35m"},
                                        {BOLD, "\033[1;33m"},
                                        {ERROR, "\033[1;38;2;205;92;92m"},
                                        {YELLOW, "\e[0;33m"},
                                        {GREEN, "\e[0;32m"},
                                        {MAGENTA, "\e[0;35m"},
                                        {WHITE, "\e[0;37m"},
                                        {RED, "\e[0;31m"}}}};
  return MAP;
}
template <_style_t S, typename T>
  requires(std::formattable<T, char>)
constexpr std::string colorizer::colorize(T&& t) {
  return std::format("{}{}{}", style_t(S).pre, t, RESET_ANSI);
}
template <typename T>
constexpr std::string colorizer::header_style(T&& t) {
  return colorize<_style_t::HEADER>(t);
}
template <typename T>
constexpr std::string colorizer::bold_style(T&& t) {
  return colorize<_style_t::BOLD>(t);
}
template <typename T>
constexpr std::string colorizer::error_style(T&& t) {
  return colorize<_style_t::ERROR>(t);
}

} // namespace cmm::strings
