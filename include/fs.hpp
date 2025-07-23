#pragma once

#include "traits.hpp"
#include <fcntl.h>
#include <filesystem>

#define LOG_PATH() \
  REGISTER_INFO("Current path: {}", fs::current_path().string());

namespace cmm::fs {
using std::filesystem::absolute;
using std::filesystem::current_path;
using std::filesystem::exists;
using std::filesystem::path;

enum class filetype : uint8_t { SOURCE, ASSEMBLY, OBJECT, BINARY };

template <typename T>
class file {
public:
  // Queries
  [[nodiscard]] bool exists() const noexcept { return fs::exists(m_path); }
  [[nodiscard]] const fs::path& path() const noexcept { return m_path; }
  [[nodiscard]] fs::path filename() const noexcept { return m_path.filename(); }
  [[nodiscard]] fs::path absolute() const noexcept {
    return std::filesystem::absolute(m_path);
  }

  [[nodiscard]] file<T> replace_extension(fs::path ext) const {
    auto new_path = m_path;
    return {new_path.replace_extension(ext)};
  }

  void remove() const { std::filesystem::remove(m_path); }

  template <StrSource Src>
  operator Src() const {
    return path().string();
  }
  operator T&() { return static_cast<T&>(*this); }

  friend T;
  friend std::formatter<file>;

private:
  fs::path m_path;

  // Constructor from string is implicit
  // bc fs::path(std::string) is not explicit
  file(const fs::path& p)
      : m_path(p) {}
  file(fs::path&& p)
      : m_path(std::move(p)) {}
};

class ifile : public file<ifile> {
public:
  ifile(const fs::path&);
  ifile(fs::path&&);

  [[nodiscard]] std::string_view content() const noexcept { return m_content; }

private:
  std::string m_content;
  [[nodiscard]] std::string read() noexcept;
};

class ofile : public file<ofile> {
public:
  ofile(const fs::path&);
  ofile(fs::path&&);

  void write(std::string_view src) const;
};

path executable();
path executable_dir();
}; // namespace cmm::fs
