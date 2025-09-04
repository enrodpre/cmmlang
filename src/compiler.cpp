#include "compiler.hpp"

#include <format>
#include <optional>
#include <ranges>
#include <string>

#include "ast.hpp"
#include "common.hpp"
#include "ir.hpp"
#include "lexer.hpp"
#include "os.hpp"
#include "parser.hpp"
#include "token.hpp"

namespace cmm {

using namespace fs;

source_code::source_code(const fs::ifile& input)
    : m_code(input.content()),
      m_file(input) {}

std::pair<std::size_t, std::size_t> source_code::to_coordinates(cmm::location loc) const {
  if (loc.start > m_code.size()) {
    throw std::out_of_range("n is out of range of the text");
  }

  std::size_t line = 1;
  std::size_t col  = 1;

  for (unsigned int i = 0; i < loc.start; ++i) {
    if (m_code[i] == '\n') {
      ++line;
      col = 1;
    } else {
      ++col;
    }
  }

  return {line, col};
}
[[nodiscard]] std::string_view source_code::get_code() const { return m_code; }
[[nodiscard]] std::string source_code::get_filename() const { return m_file.filename().c_str(); }
bool source_code::is_valid(const location& loc) const {
  auto line                = get_line(loc);
  const auto& [start, end] = loc;
  return (start <= line.first) && (end <= line.second);
}

[[nodiscard]] std::string source_code::get_nth_line(const location& loc) const {
  const auto& [n, c] = to_coordinates(loc);
  std::istringstream in(m_code);
  std::string line;
  size_t i = 1;
  while (std::getline(in, line)) {
    if (i++ == n)
      return line;
  }
  throw cmm::error("aaa");
}
std::pair<size_t, size_t> source_code::get_line(const location& loc) const {
  const auto& [start, end] = loc;

  // Get last \n before start
  auto right_before = m_code.substr(0, start).rfind('\n');
  if (right_before == std::string::npos) {
    throw cmm::error("No newline found");
  }

  auto right_after = m_code.substr(end, std::string::npos).find('\n');
  if (right_after == std::string::npos) {
    throw cmm::error("No newline found");
  }

  return std::make_pair(right_before + 1, end + right_after);
}

std::tuple<std ::string, std::string, std::string> source_code::get_line_chunked(
    const location& loc) const {
  auto [start, end]       = loc;
  auto str                = get_nth_line(loc);
  const auto& [line, col] = to_coordinates(loc);
  auto left               = m_code.substr(col);
  auto middle             = m_code.substr(col, end);
  auto right              = m_code.substr(end);

  return {left, middle, right};
}
std::string source_code::build_full_location(const location& loc) {
  const auto& [line, column] = to_coordinates(loc);
  return std::format("{}:{}:{}", get_filename(), line, column);
}
compiler::compiler(const fs::path& input, const std::string& output)
    : m_source_code(input),
      m_output_filename(output) {}

void compiler::preprocess(const std::string&) { REGISTER_WARN("Preprocessor disabled"); }

ofile compiler::compile(const source_code& src) {
  lexer lexer_instance(src.get_code());
  auto tokens = lexer_instance.tokenize();

  parser::parser parser(tokens);
  auto compound               = parser.parse();

  ir::compilation_unit& cunit = ir::compilation_unit::instance();

  fs::ofile asm_file(m_output_filename);
  asm_file = asm_file.replace_extension("asm");

  try {
    auto asm_code = cunit.compile(compound, &src);
    asm_file.write(asm_code);
    return asm_file;
  } catch (const compilation_error& e) {
    if (e.loc.has_value()) {
      auto stackframe = cunit.ast->stackframe();
      auto lines      = m_source_code.build_compilation_error(stackframe, e.what(), e.loc.value());
      for (const auto& line : lines) {
        WRITE_STDOUT("{}\n", line);
      }
    } else {
      REGISTER_ERROR("{}", e.what());
    }
    exit(1);
  }
}

ofile compiler::assemble(ofile& asm_file) {
  os::execute(std::format("nasm -felf64 -g {}", asm_file.path().string()));

  return asm_file.replace_extension("o");
}

ofile compiler::link(ofile& obj_file) {
  auto binary = obj_file.replace_extension("");
  os::execute(std::format("ld -o {} {}", binary.path().string(), obj_file.path().string()));
  obj_file.remove();
  return binary;
}

fs::ofile compiler::run() {
  LOG_PATH();
  REGISTER_INFO("Compiling {}", m_source_code.get_filename());

  preprocess();
  auto asm_file     = compile(m_source_code);

  auto obj_file     = assemble(asm_file);
  const auto binary = link(obj_file);

  REGISTER_INFO(
      "Succesfully compiled {} into {}", m_source_code.get_filename(), binary.path().string());

  return binary;
}

void compiler::throw_linking_error(const std::string& err) {
  REGISTER_ERROR("{}\n", err);
  exit(1);
}

std::vector<std::string> source_code::build_compilation_error(
    const std::vector<const ast::decl::function::definition*>&,
    std::string_view err,
    const location& loc) {
  std::vector<std::string> message;
  auto full_loc = build_full_location(loc);
  auto first_line =
      std::format("{}: {} {}", full_loc, log::apply("error: ", log::style_t::ERROR), err);
  auto [left, error, right] = get_line_chunked(loc);
  auto formatted_error      = log::apply(error, log::style_t::ERROR);

  auto left_size            = left.size();
  auto second_line          = std::format("{:6}{}{}", left, formatted_error, right);
  auto third_line           = std::format("{}{}", std::string(left_size, ' '), "^");

  const size_t margin_left  = 6;
  message.push_back(first_line);
  message.push_back(std::format(
      "{}{} |{}", std::string(margin_left - 2, ' '), to_coordinates(loc).first, second_line));
  message.push_back(std::format("{}|{}",
                                std::string(margin_left, ' '),
                                log::apply(third_line, log::style_t::ERROR_UNDERLINE)));

  return message;
}
} // namespace cmm
