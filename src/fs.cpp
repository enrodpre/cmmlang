#include "fs.hpp"

#include <filesystem>
#include <fstream>
#include <iterator>

namespace cmm::fs {

std::filesystem::path executable() { return std::filesystem::canonical("/proc/self/exe"); }
std::filesystem::path executable_dir() { return executable().parent_path(); }

std::string read(const std::filesystem::path& t_path) {
  std::ifstream cmm_file;
  cmm_file.open(t_path);
  return {std::istreambuf_iterator<char>(cmm_file), {}};
}

void write(const std::filesystem::path& t_path, std::string_view t_src) {
  std::ofstream f(t_path, std::ios_base::out | std::ios_base::trunc);
  f.write(t_src.data(), static_cast<long>(t_src.size()));
  f.close();
}

}; // namespace cmm::fs
