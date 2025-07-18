#include "preprocessor.hpp"
#include "fs.hpp"
#include "strings.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace cmm {
using svmatch        = cmm::strings::svmatch;

Preprocessor::Preprocessor(std::string src)
    : src(std::move(src)),
      pointer(0),
      m_row(0)
{}

std::string&& Preprocessor::preprocess()
{
  remove_single_line_comments();
  remove_block_comments();
  process_includes();
  process_defines();

  return std::move(this->src);
}

std::string_view Preprocessor::string() const
{
  return cmm::cstring(src).substr(pointer);
}

void Preprocessor::replace(size_t start, size_t end, cstring str)
{
  src = src.replace(src.begin() + start, src.begin() + end, str);
}

void Preprocessor::remove_single_line_comments()
{
  auto next_line_comment = src.find(LINE_COMMENT);
  while (next_line_comment != std::string::npos) {
    // find next newline character starting from single line comment
    auto end_line = src.find(NEWLINE_CHARACTER, next_line_comment);
    replace(next_line_comment, end_line, "");

    next_line_comment = src.find(LINE_COMMENT);
  }
}

void Preprocessor::remove_block_comments()
{
  auto next_block_comment = src.find(OPEN_BLOCK_COMMENT);
  while (next_block_comment != std::string::npos) {
    auto end_block_comment = src.find(CLOSE_BLOCK_COMMENT, next_block_comment);
    replace(next_block_comment, end_block_comment + 2, "");

    next_block_comment = src.find(OPEN_BLOCK_COMMENT);
  }
}

void Preprocessor::process_includes()
{
  pointer         = 0;
  size_t next_pos = string().find("#include", pointer);
  while (next_pos != std::string::npos) {

    pointer = next_pos;
    if (pointer != 0 && src.at(pointer - 1) != '\n') {
      // throw cmm::exception::preprocessor_error({__FILE__, pointer, 0, 0});
    }

    auto remaining = string();
    size_t next_nl = strings::find_next_newline(remaining);
    auto line      = remaining.substr(next_nl);
    svmatch include_match;
    bool success = cmm::strings::match_regex_sv(line, INCLUDE_PATTERN, include_match);
    if (!success || include_match.empty()) {
      /* throw exception::preprocessor_error("Include directives must go in a line
       * itself"); */
    }

    std::string filename = include_match[1];
    fs::ifile included_file(filename);
    size_t start_line = include_match.position();
    /* BINARY_ASSERT(next_pos, ==, start_line); */

    auto end_line = start_line + include_match.length();

    // + 1 because we want to remove newline as well
    replace(start_line, end_line + 1, included_file.content());
  }
  next_pos = string().find("#include", pointer);
}

void Preprocessor::process_defines()
{
  pointer       = 0;
  auto next_pos = string().find("#define", pointer);
  while (next_pos != std::string::npos) {
    pointer        = next_pos;
    auto prev_char = src.at(pointer - 1);
    if (prev_char != '\n') {
      /* throw cmm::exception::preprocessor_error({pointer, 0}); */
    }

    auto remaining = string();
    size_t next_nl = strings::find_next_newline(remaining);
    auto line      = remaining.substr(next_nl);
    svmatch include_match;
    bool ready = strings::match_regex_sv(line, DEFINE_PATTERN, include_match);
    if (ready && include_match.ready() && !include_match.empty()) {
      strings::svsub_match name = include_match[1];
      // auto macro = define_match[2];

      // Remove define
      auto start_line = include_match.position();
      auto end_line   = start_line + include_match.length();
      replace(start_line, end_line + 1, "");

      auto pos = string().find(name.str(), pointer);
      if (pos != std::string::npos) {
        auto end_macro = pos + name.length();
        src.replace(pos, end_macro, "");
      }
    }

    next_pos = string().find('#', pointer);
  }
}
} // namespace cmm
