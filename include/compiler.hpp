#pragma once

#include <cstddef>
#include <filesystem>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

#include "ast.hpp"
#include "common.hpp"
#include "macros.hpp"

namespace cmm {
struct location;

class source_code {
public:
  source_code(const std::filesystem::path&, std::string);
  NOT_COPYABLE_CLS(source_code);

  [[nodiscard]] std::string_view get_code() const { return m_code; }
  [[nodiscard]] const std::filesystem::path& get_input() const { return m_input; }
  [[nodiscard]] const std::filesystem::path& get_output() const { return m_output; }
  [[nodiscard]] std::string get_filename() const { return m_input.filename(); }
  std::string_view get_compiled() const { return m_compiled; }
  void set_compiled(std::string t_compiled) { m_compiled = t_compiled; }

  std::vector<std::string> build_compilation_error(
      const std::vector<const ast::decl::function::definition*>&,
      std::string_view,
      const location&) const;

private:
  std::string m_code;
  std::filesystem::path m_input;
  std::string m_compiled;
  std::filesystem::path m_output;

  std::string build_full_location(const location&) const;
  std::tuple<size_t, size_t, size_t> to_coordinates(const cmm::location&) const;
  [[nodiscard]] std::string_view get_nth_line(size_t) const;
  [[nodiscard]] std::tuple<std::string_view, std::string_view, std::string_view> get_line_chunked(
      const location&) const;
};

struct configuration {
  STATIC_CLS(configuration);

  static inline bool keep_preprocessed = false;
  static inline bool keep_object       = false;
  static inline bool keep_assembly     = true;
};

class compiler {
public:
  STATIC_CLS(compiler);

  static int compile(std::filesystem::path, std::string);

  static void throw_linking_error(const std::string&);

private:
  static void preprocess(source_code&);
  static int compile(source_code&);
  static int assemble(const std::filesystem::path&);
  static int link(const std::filesystem::path&, const std::filesystem::path&);
};

}; // namespace cmm
