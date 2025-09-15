#pragma once

#include <cstdint>
#include <format>
#include <spdlog/spdlog.h>

namespace log {

enum class style_t : uint8_t {
  HEADER,
  BOLD,
  ERROR,
  ERROR_UNDERLINE,
  NORMAL,
  RED,
  MAGENTA,
  YELLOW,
  GREEN,
  DARK_RED,
  WHITE_SMOKE,
  WHITE
};

template <std::formattable<char> T>
constexpr std::string apply(const T&, style_t);

enum class Level : uint8_t { NONE = 0, ERROR, WARN, INFO, DEBUG, TRACE };
} // namespace log
#ifndef LOG_LEVEL
#  define LOG_LEVEL TRACE_LEVEL
#endif

#define TRACE_LEVEL 5
#define DEBUG_LEVEL 4
#define INFO_LEVEL  3
#define WARN_LEVEL  2
#define ERROR_LEVEL 1
#define NONE_LEVEL  0

#if LOG_LEVEL == 0
#  define REGISTER_LOG(lvl, file, header_color, formatter_string, ...)
#  define WRITE_STDOUT(fmt_string, ...)
#else
#  define WRITE_STDOUT(fmt_string, ...) std::print(stdout, fmt_string, ##__VA_ARGS__)
#  define REGISTER_LOG(lvl, file, header_color, formatter_string, ...)                            \
    std::print(file, "[{}:{} {}] ", __FILE_NAME__, __LINE__, cmm::log::apply(lvl, header_color)); \
    std::println(file, formatter_string, ##__VA_ARGS__)
#endif

#if LOG_LEVEL >= ERROR_LEVEL
#  define REGISTER_ERROR(std_string, ...)                                                     \
    REGISTER_LOG(STRINGIZE_IMPL(ERROR), stderr, log::style_t::RED, std_string, ##__VA_ARGS__)
#else
#  define REGISTER_ERROR(std_string, ...)
#endif

#if LOG_LEVEL >= WARN_LEVEL
#  define REGISTER_WARN(std_string, ...)                                                         \
    REGISTER_LOG(STRINGIZE_IMPL(WARN), stdout, log::style_t::MAGENTA, std_string, ##__VA_ARGS__)
#else
#  define REGISTER_WARN(std_string, ...)
#endif

#if LOG_LEVEL >= INFO_LEVEL
#  define REGISTER_INFO(std_string, ...)                                                       \
    REGISTER_LOG(STRINGIZE_IMPL(INFO), stdout, log::style_t::GREEN, std_string, ##__VA_ARGS__)
#else
#  define REGISTER_INFO(std_string, ...)
#endif

#if LOG_LEVEL >= DEBUG_LEVEL
#  define REGISTER_DEBUG(std_string, ...)                                                        \
    REGISTER_LOG(STRINGIZE_IMPL(DEBUG), stdout, log::style_t::YELLOW, std_string, ##__VA_ARGS__)

#else
#  define REGISTER_DEBUG(std_string, ...)
#endif

#if LOG_LEVEL >= TRACE_LEVEL
#  if DEBUG_MEMORY
#    define MEMORY_TRACE(std_string, ...)                                                    \
      REGISTER_LOG(                                                                          \
          STRINGIZE_IMPL(TRACE), stdout, std::color::white_smoke, std_string, ##__VA_ARGS__)
#  else
#    define MEMORY_TRACE(std_string, ...)
#  endif
#  define REGISTER_TRACE(std_string, ...) \
    REGISTER_LOG(STRINGIZE_IMPL(TRACE), stdout, cmm::log::style_t::WHITE, std_string, ##__VA_ARGS__)
#else
#  define REGISTER_TRACE(std_string, ...)
#endif

constexpr void initialize_logging() { spdlog::set_level(spdlog::level::trace); }
