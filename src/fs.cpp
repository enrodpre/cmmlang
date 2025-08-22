#include "fs.hpp"

#include <filesystem>
#include <fstream>
#include <iterator>

#include "common.hpp"

namespace cmm::fs {

path executable() {
  return std::filesystem::canonical("/proc/self/exe");
}

path executable_dir() {
  return executable().parent_path();
}

ifile::ifile(const fs::path& p)
    : file<ifile>(p),
      m_content(read()) {}

ifile::ifile(fs::path&& p)
    : file<ifile>(std::move(p)),
      m_content(read()) {}

[[nodiscard]] std::string ifile::read() noexcept {
  std::ifstream cmm_file;
  cmm_file.open(m_path);
  return {std::istreambuf_iterator<char>(cmm_file), {}};
}

ofile::ofile(const fs::path& p)
    : file<ofile>(p) {}

ofile::ofile(fs::path&& p)
    : file<ofile>(std::move(p)) {}

void ofile::write(cstring src) const {
  std::ofstream file(m_path, std::ios_base::out | std::ios_base::trunc);
  file.write(src.data(), static_cast<long>(src.size()));
  file.close();
}

}; // namespace cmm::fs
