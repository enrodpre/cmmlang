#pragma once

#include <string>
#include <string_view>

#include "ast.hpp"
#include "common.hpp"
#include "fs.hpp"

namespace cmm {

class source_code {
public:
  source_code(const fs::ifile&);

  [[nodiscard]] const std::string& get_code() const;
  [[nodiscard]] std::string get_filename() const;
  [[nodiscard]] bool is_valid(const location&) const;
  [[nodiscard]] std::pair<size_t, size_t> get_line(const location&) const;
  [[nodiscard]] std::string get_chunk(const location&) const;
  [[nodiscard]] std::tuple<std::string, std::string, std::string> get_line_chunked(
      const location&) const;
  std::pair<size_t, size_t> to_coordinates(cmm::location) const;

  std::vector<std::string> build_compilation_error(
      const std::vector<const ast::decl::function::definition*>&,
      std::string_view,
      const location&);

private:
  std::string m_code;
  std::filesystem::path m_file;

  std::string build_full_location(const location&);
};

class compiler {

public:
  compiler(const fs::path&, const std::string&);
  compiler(compiler&&)                 = delete;
  compiler& operator=(compiler&&)      = delete;
  compiler(const compiler&)            = delete;
  compiler& operator=(const compiler&) = delete;
  ~compiler()                          = default;

  static void preprocess(const std::string& = "");
  fs::ofile compile(const source_code&);
  static fs::ofile assemble(fs::ofile& file);
  static fs::ofile link(fs::ofile& file);
  fs::ofile run();

  static void throw_linking_error(const std::string&);

private:
  source_code m_source_code;
  cstring m_output_filename;
};

}; // namespace cmm
