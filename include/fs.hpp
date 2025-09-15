#pragma once

#include <fcntl.h>
#include <filesystem>
#include <string>
#include <string_view>

template <typename CharT>
struct std::formatter<std::filesystem::path, CharT> : std::formatter<string_view> {
  template <typename Ctx>
  auto format(const std::filesystem::path& t_path, Ctx& t_ctx) const -> decltype(t_ctx.out()) {
    return std::formatter<string_view>::format(t_path.string(), t_ctx);
  }
};

static_assert(std::formattable<std::filesystem::path, char>);

namespace cmm::fs {

void write(const std::filesystem::path&, std::string_view);
std::string read(const std::filesystem::path&);
std::filesystem::path executable();
std::filesystem::path executable_dir();
}; // namespace cmm::fs
