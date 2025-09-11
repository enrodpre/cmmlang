#include "compiler.hpp"

#include <format>
#include <iterator>
#include <optional>
#include <ranges>
#include <stdexcept>
#include <stdlib.h>
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

  return {line, col, loc.end - loc.start};
}

[[nodiscard]] std::string_view source_code::get_code() const { return m_code; }
[[nodiscard]] path source_code::get_input() const { return m_input; }
[[nodiscard]] path source_code::get_output() const { return m_output; }
[[nodiscard]] std::string source_code::get_filename() const { return m_input.filename(); }

[[nodiscard]] std::string_view source_code::get_nth_line(size_t nth) const {
  auto split = std::views::split(get_code(), '\n') | TO_VEC;
  if (std::ranges::empty(split)) {
    throw std::range_error("error nth line");
  }
  auto subrange = *split.begin();
  return std::string_view{std::ranges::subrange(subrange)};
}

std::tuple<std::string_view, std::string_view, std::string_view> source_code::get_line_chunked(
    const location& loc) const {
  const auto& [line, col_start, col_end] = to_coordinates(loc);
  auto str                               = get_nth_line(line);
  auto left                              = str.substr(0, col_start);
  auto middle                            = str.substr(col_start, col_end - col_start);
  auto right                             = str.substr(col_end);

  return {left, middle, right};
}
std::string source_code::build_full_location(const location& loc) const {
  const auto& [line, start, end] = to_coordinates(loc);
  return std::format("{}:{}:{}", get_filename(), line, start);
}

void compiler::preprocess(const source_code&) { REGISTER_WARN("Preprocessor disabled"); }

path compiler::compile(const source_code& ctx) {
  lexer lexer_instance(ctx.get_code());
  auto tokens = lexer_instance.tokenize();

  parser::parser parser(tokens);
  auto compound               = parser.parse();

  ir::compilation_unit& cunit = ir::compilation_unit::instance();

  auto asm_file               = ctx.get_output();
  asm_file.replace_extension("asm");

  try {
    auto asm_code = cunit.compile(compound, &ctx);
    cmm::fs::write(asm_file, asm_code);
    return asm_file;
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
    exit(1);
  }
}

path& compiler::assemble(std::filesystem::path& compiled) {
  os::execute(std::format("nasm -felf64 -g {}", compiled.string()));

  return compiled.replace_extension("o");
}

path& compiler::link(path& obj_file) {
  auto binary = obj_file;
  os::execute(std::format("ld -o {} {}", obj_file.replace_extension("").string(), binary.string()));
  if (!configuration::keep_object) {
    remove(binary);
  }

  return obj_file;
}

path compiler::compile(path t_path, std::string t_out) {
  source_code context{t_path, t_out};

  REGISTER_INFO("Compiling {}", context.get_input().string());

  preprocess(context);
  auto asm_file     = compile(context);

  auto obj_file     = assemble(asm_file);
  const auto binary = link(obj_file);

  REGISTER_INFO("Succesfully compiled {} into {}", context.get_filename(), binary.string());

  return binary;
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
  auto full_loc = build_full_location(loc);
  auto first_line =
      std::format("{}: {} {}", full_loc, log::apply("error: ", log::style_t::ERROR), err);
  auto [left, error, right] = get_line_chunked(loc);
  auto formatted_error      = log::apply(error, log::style_t::ERROR);

  auto left_size            = left.size();
  auto second_line          = std::format("{}{}{}", left, formatted_error, right);
  auto third_line           = std::format("{}{}", std::string(left_size, ' '), "^");

  const size_t margin_left  = 6;
  message.push_back(first_line);
  message.push_back(std::format("{}{} |{}",
                                std::string(margin_left - 2, ' '),
                                std::get<0>(to_coordinates(loc)),
                                second_line));
  message.push_back(std::format("{}|{}",
                                std::string(margin_left, ' '),
                                log::apply(third_line, log::style_t::ERROR_UNDERLINE)));

  return message;
}
} // namespace cmm
