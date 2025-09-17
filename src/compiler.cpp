#include "compiler.hpp"

#include <cstdlib>
#include <fmt/color.h>
#include <format>
#include <iterator>
#include <optional>
#include <ranges>
#include <stdexcept>
#include <string>

#include "ast.hpp"
#include "common.hpp"
#include "fs.hpp"
#include "ir.hpp"
#include "lexer.hpp"
#include "os.hpp"
#include "parser.hpp"
#include "token.hpp"

namespace cmm {

using namespace std::filesystem;
source_code::source_code(const path& input, std::string t_out)
    : m_code(fs::read(input)),
      m_input(input),
      m_output(t_out) {}

std::tuple<size_t, size_t, size_t> source_code::to_coordinates(const cmm::location& loc) const {
  if (loc.end > m_code.size()) {
    throw std::out_of_range("n is out of range of the text");
  }

  uint32_t line = 1;
  uint32_t col  = 1;
  for (uint32_t i = 0; i < loc.start && i < get_code().size(); ++i) {
    if (get_code()[i] == '\n') {
      ++line;
      col = 1;
    } else {
      ++col;
    }
  }

  return {line, col, col + loc.end - loc.start};
}

[[nodiscard]] std::string_view source_code::get_nth_line(size_t nth) const {
  auto split = std::views::split(get_code(), '\n') | TO_VEC;
  if (std::ranges::empty(split)) {
    throw std::range_error("error nth line");
  }
  auto subrange = split.begin();
  while (--nth > 0) {
    subrange++;
  };
  return std::string_view{*subrange};
}

std::tuple<std::string_view, std::string_view, std::string_view> source_code::get_line_chunked(
    const location& loc) const {
  const auto& [line, col_start, col_end] = to_coordinates(loc);
  auto str                               = get_nth_line(line);
  auto left                              = str.substr(0, col_start - 1);
  auto middle                            = str.substr(col_start - 1, col_end - col_start);
  auto right                             = str.substr(col_end - 1);

  return {left, middle, right};
}
std::string source_code::build_full_location(const location& loc) const {
  const auto& [line, start, end] = to_coordinates(loc);
  return std::format("{}:{}:{}", get_filename(), line, start);
}

void compiler::preprocess(source_code&) { REGISTER_WARN("Preprocessor disabled"); }

int compiler::compile(source_code& ctx) {
  lexer lexer_instance(ctx.get_code());
  auto tokens = lexer_instance.tokenize();

  parser::parser parser(tokens);
  ast::translation_unit* tu = parser.parse();

  ir::compilation_unit cunit(tu, &ctx);

  try {
    cunit.compile();
    return os::SUCCESS;
  } catch (const compilation_error& e) {
    if (e.loc.has_value()) {
      auto stackframe = cunit.ast->stackframe();
      auto lines      = ctx.build_compilation_error(stackframe, e.what(), e.loc.value());
      for (const auto& line : lines) {
        WRITE_STDOUT("{}\n", line);
      }
    } else {
      REGISTER_ERROR("{}", e.what());
    }
    return os::COMPILATION_ERROR;
  }
}

int compiler::assemble(const path& compiled) {
  int res = os::execute(std::format("nasm -felf64 -g {}", compiled.string()));
  if (!configuration::keep_assembly) {
    remove(compiled);
  }
  return res;
}

int compiler::link(const path& t_obj, const path& t_binary) {
  int result = os::execute(std::format("ld -o {} {}", t_binary, t_obj));
  if (!configuration::keep_object) {
    remove(t_obj);
  }
  return result;
}

int compiler::compile(path t_path, std::string t_out) {
  source_code context{t_path, t_out};

  REGISTER_INFO("Compiling {}", context.get_input());

  preprocess(context);
  auto compiler_res = compile(context);
  if (compiler_res != 0) {
    return compiler_res;
  }
  auto compiled = context.get_compiled();
  auto out      = context.get_output();
  auto asm_file = out;
  asm_file.replace_extension("asm");
  fs::write(asm_file, compiled);

  auto asm_res = assemble(asm_file);
  if (asm_res != 0) {
    REGISTER_ERROR("Error assembling");
    return os::ASSEMBLER_ERROR;
  }

  auto link_res = link(out.replace_extension("o"), context.get_output());
  if (link_res != 0) {
    REGISTER_ERROR("Error linking");
    return os::LINKING_ERROR;
  }

  REGISTER_INFO("Succesfully compiled {} into {}", context.get_filename(), context.get_output());

  return 0;
}

void compiler::throw_linking_error(const std::string& err) {
  REGISTER_ERROR("{}\n", err);
  exit(1);
}

std::vector<std::string> source_code::build_compilation_error(
    const std::vector<const ast::decl::function::definition*>&,
    std::string_view err,
    const location& loc) const {
  std::vector<std::string> message;
  auto fmt_error_style = fmt::emphasis::bold | fmt::fg(fmt::color::red);
  auto full_loc        = build_full_location(loc);
  auto first_line =
      fmt::format("{}: {} {}", full_loc, fmt::styled("error: ", fmt_error_style), err);
  auto [left, error, right] = get_line_chunked(loc);

  auto left_size            = left.size();
  auto second_line = fmt::format("{}{}{}", left, fmt::styled(error, fmt_error_style), right);
  auto third_line  = std::format("{}{}", std::string(left_size, ' '), "^");

  const size_t margin_left = 6;
  message.push_back(first_line);
  message.push_back(std::format("{}{} |{}",
                                std::string(margin_left - 2, ' '),
                                std::get<0>(to_coordinates(loc)),
                                second_line));
  message.push_back(
      fmt::format("{}|{}",
                  std::string(margin_left, ' '),
                  fmt::styled(third_line, fmt::emphasis::underline | fmt::fg(fmt::color::red))));

  return message;
}
} // namespace cmm
