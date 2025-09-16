#pragma once

#include <spdlog/sinks/ansicolor_sink.h>
#include <spdlog/spdlog.h>

#define REGISTER_CRITICAL(...) SPDLOG_CRITICAL(__VA_ARGS__)
#define REGISTER_ERROR(...)    SPDLOG_ERROR(__VA_ARGS__)
#define REGISTER_WARN(...)     SPDLOG_WARN(__VA_ARGS__)
#define REGISTER_INFO(...)     SPDLOG_INFO(__VA_ARGS__)
#define REGISTER_DEBUG(...)    SPDLOG_DEBUG(__VA_ARGS__)
#define REGISTER_TRACE(...)    SPDLOG_TRACE(__VA_ARGS__)
#define WRITE_STDOUT(FMT, ...) fmt::print(FMT, ##__VA_ARGS__)

// template <std::formattable<char> T>
// struct fmt_adapter : fmt::formatter<std::string_view> {
//
//   template <typename FormatContext>
//   auto format(const T& value, FormatContext& ctx) const {
//     return fmt::format_to(ctx.out(), std::format("{}", value));
//   }
// };
//
namespace cmm::logging {
#define DEFINE_COLOR(NAME, ANSI) constexpr auto NAME = ANSI

DEFINE_COLOR(HEADER, "\033[40;1;35m");
DEFINE_COLOR(BOLD, "\033[1;33m");
DEFINE_COLOR(ERROR, "\033[1;38;2;205;92;92m");
DEFINE_COLOR(ERROR_UNDERLINE, "\033[38;2;239;137;165m");
DEFINE_COLOR(RED, "\033[0;31m");
DEFINE_COLOR(MAGENTA, "\033[0;35m");
DEFINE_COLOR(LIGHT_GRAY, "\033[0;37m");
DEFINE_COLOR(GREEN, "\033[0;32m");
DEFINE_COLOR(YELLOW, "\033[0;33m");
DEFINE_COLOR(WHITE, "\033[0;37m");

constexpr void initialize_logging() {
  // Create a colored console sink
  auto console_sink = std::make_shared<spdlog::sinks::ansicolor_stdout_sink_mt>();

  // Set the pattern: [file:line level] message
  // %s = source filename, %# = line number, %^%l%$ = colored level, %v = message
  console_sink->set_pattern("[%s:%# %^%l%$] %v");

  // Create logger with the sink
  auto logger = std::make_shared<spdlog::logger>("main", console_sink);

  // Set custom colors for each level
  console_sink->set_color(spdlog::level::trace, LIGHT_GRAY); // WHITE_SMOKE (light gray)
  console_sink->set_color(spdlog::level::debug, YELLOW);     // YELLOW
  console_sink->set_color(spdlog::level::info, GREEN);       // GREEN
  console_sink->set_color(spdlog::level::warn, MAGENTA);     // MAGENTA
  console_sink->set_color(spdlog::level::err, RED);          // RED
  console_sink->set_color(spdlog::level::critical, ERROR);   // RED

  // Set as default logger
  spdlog::set_default_logger(logger);

  // Enable source location (file:line) - IMPORTANT!
  spdlog::set_pattern("[%s:%# %^%l%$] %v");

  spdlog::set_level(spdlog::level::trace);
}

} // namespace cmm::logging
