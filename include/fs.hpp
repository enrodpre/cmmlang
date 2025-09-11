#pragma once

#include <fcntl.h>
#include <filesystem>
#include <string>
#include <string_view>

#define LOG_PATH() REGISTER_INFO("Current path: {}", fs::current_path().string());

namespace cmm::fs {

void write(const std::filesystem::path&, std::string_view);
std::string read(const std::filesystem::path&);
std::filesystem::path executable();
std::filesystem::path executable_dir();
}; // namespace cmm::fs
