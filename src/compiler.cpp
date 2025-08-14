#include "compiler.hpp"
#include "common.hpp"
#include "fs.hpp"
#include "ir.hpp"
#include "lexer.hpp"
#include "os.hpp"
#include "parser.hpp"
#include <cpptrace/basic.hpp>
#include <string>

namespace cmm {

using namespace fs;

compiler::compiler(const config& config_, const fs::path& input, const std::string& output)
    : m_config(config_),
      m_source_code(input),
      m_output_filename(output) {}

void compiler::preprocess(const std::string&) {
  REGISTER_WARN("Preprocessor disabled");
  /* auto preprocessor   = code == "" ? Preprocessor(m_source_code) :
   * Preprocessor(code);
   */
  /* m_preprocessed_code = std::move(preprocessor.preprocess()); */
  /**/
#if SAVE_PREPROCESSED
  auto output = m_output;
  fs::write(output.replace_extension("preprocessed"), m_preprocessed_code);
#endif
}

ofile compiler::compile(const source_code& src) {
  lexer lexer_instance(src.get_code());
  auto tokens = lexer_instance.tokenize();

  if (m_config.dump_tokens) {
    // std::print("{}", tokens);
  }

  parser::parser parser(tokens);
  auto compound = parser.parse();

  if (m_config.dump_ast) {
    // std::print("{}\n", compound.join('\n'));
  }
  ir::compilation_unit& cunit = ir::compilation_unit::instance();

  fs::ofile asm_file(m_output_filename);
  asm_file = asm_file.replace_extension("asm");

  try {
    auto asm_code = cunit.compile(compound, &src);
    asm_file.write(asm_code);
    return asm_file;
  } catch (const compilation_error& e) {
    if (e.loc.has_value()) {
      throw_compilation_error(e.what(), e.loc.value());
    } else {
      REGISTER_ERROR("{}", e.what());
    }
    os::error(e.status);
  }
}

ofile compiler::assemble(ofile& asm_file) {
  os::execute(std::format("nasm -felf64 -g {}", asm_file.path().string()));

  if constexpr (!cmm::config::assembly) {
    // asm_file.remove();
  }
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

  if (m_config.dump_memory) {
    // memory::Allocator::get().report_statistics();
  }
  return binary;
}

void compiler::throw_linking_error(const std::string& err) {
  REGISTER_ERROR("{}\n", err);
  cmm::os::error(os::status::LINKING_ERROR);
}

void compiler::throw_compilation_error(std::string_view err, const location& loc) {
  auto text_header =
      std::format("{}:{}:{}:", m_source_code.get_filename(), loc.rows.start, loc.cols.start);
  auto styled_text_header = log::apply(text_header, log::style_t::HEADER);
  auto first_line =
      std::format("{} {} {}", styled_text_header, log::apply("error: ", log::style_t::ERROR), err);

  auto [left, error, right] = m_source_code.get_line_chunked(loc);
  auto formatted_error      = log::apply(error, log::style_t::ERROR);
  auto formatter_error_line = std::format("{}{}{}\n", left, formatted_error, right);
  auto second_line          = std::format("   {} |    {}", loc.rows.start, formatter_error_line);
  auto formatted_msg        = std::format("{}\n{}", first_line, second_line);

  REGISTER_ERROR("{}", formatted_msg);
}
} // namespace cmm
