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
  const auto& [start, end] = get_line(loc);
  auto before              = get_code().substr(0, start);
  auto line                = std::ranges::count(before, '\n');
  auto column              = loc.start - start;
  // +2 because is
  // 1. is 0 based
  // 2. the fist line does not have \n
  return {line + 2, column};
}
[[nodiscard]] const std::string& source_code::get_code() const { return m_code; }
[[nodiscard]] std::string source_code::get_filename() const { return m_file.filename().c_str(); }
bool source_code::is_valid(const location& loc) const {
  auto line                = get_line(loc);
  const auto& [start, end] = loc;
  return (start <= line.first) && (end <= line.second);
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

std::string source_code::get_chunk(const location& loc) const {
  if (!is_valid(loc)) {
    auto line = get_line(loc);
    REGISTER_ERROR("location {}, size {}", loc, line.second - line.first);
    throw std::exception();
  }

  auto [start, len] = loc;
  return m_code.substr(start, start + len);
}

std::tuple<std ::string, std::string, std::string> source_code::get_line_chunked(
    const location& loc) const {
  auto [start, end]                 = loc;
  const auto& [top_left, top_right] = get_line(loc);
  auto left                         = m_code.substr(top_left, start - top_left);
  auto middle                       = m_code.substr(start, (end - start));
  auto right                        = m_code.substr(end, top_right - end);

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
      WRITE_STDOUT("{}\n", lines | std::views::join_with('\n'));
    } else {
      REGISTER_ERROR("{}", e.what());
    }
    os::error(e.status);
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
  cmm::os::error(os::status::LINKING_ERROR);
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

  const size_t margin_left  = 6;
  auto left_size            = left.size();
  auto second_line          = std::format("{:6}{}{}", left, formatted_error, right);
  auto third_line = std::format("{:}>{}{}", "^", left_size + margin_left, second_line.size());

  message.push_back(first_line);
  message.push_back(second_line);
  // #f38ba8
  WRITE_STDOUT("{}\n", first_line);
  WRITE_STDOUT("{}\n", second_line);
  WRITE_STDOUT("{}\n", third_line);
  return message;
}
} // namespace cmm
