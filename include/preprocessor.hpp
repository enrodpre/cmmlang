#pragma once

#include <cstddef>
#include <string>
#include <string_view>

#include "common.hpp"

namespace cmm {

struct preprocessor_error : public compilation_error {
  using compilation_error::compilation_error;
};

// TOOD only is implemented the static defines
class Preprocessor {
public:
  Preprocessor(std::string);
  Preprocessor()                               = delete;
  Preprocessor& operator=(const Preprocessor&) = delete;
  Preprocessor& operator=(Preprocessor&&)      = delete;
  Preprocessor(const Preprocessor&)            = delete;
  Preprocessor(Preprocessor&&)                 = delete;

  std::string&& preprocess();
  void remove_single_line_comments();
  void remove_block_comments();
  void process_includes();
  void process_defines();

  [[nodiscard]] std::string_view string() const;

private:
  std::string src;
  size_t pointer;
  // size_t m_row;

  void replace(size_t, size_t, std::string_view);

  static constexpr char NEWLINE_CHARACTER = '\n';
  static constexpr auto INCLUDE_PATTERN   = "^\\s*#\\s*include\\s+[<\"]([^>\"]*)[>\"]\\s*";
  static constexpr auto DEFINE_PATTERN =
      "\\s*#\\s*define\\s+(\n[A-Za-z_][A-Za-z0-9_]*)\n\\s*(.*)\n$\n";
  static constexpr auto LINE_COMMENT        = "//";
  static constexpr auto OPEN_BLOCK_COMMENT  = "/*";
  static constexpr auto CLOSE_BLOCK_COMMENT = "*/";
};
} // namespace cmm
