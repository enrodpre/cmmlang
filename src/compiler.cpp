#include "compiler.hpp"
#include "common.hpp"
#include "filesystem"
#include "fs.hpp"
#include "ir.hpp"
#include "lexer.hpp"
#include "os.hpp"
#include "parser.hpp"
#include "strings.hpp"
#include <basic.hpp>
#include <iostream>
#include <string>

namespace cmm {

using namespace fs;

compiler::compiler(const config& config_,
                   const fs::path& input,
                   const std::string& output)
    : m_config(config_),
      m_source_code(input),
      m_output_filename(output) {}

void compiler::preprocess(const std::string&) {
  spdlog::warn("Preprocessor disabled");
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

ofile compiler::compile(const strings::source_code& src) {
  lexer lexer_instance(src.get_code());
  auto tokens = lexer_instance.tokenize();

  if (m_config.dump_tokens) {
    std::print("{}", tokens);
  }

  parser::parser parser(tokens);
  auto compound = parser.parse();

  if (m_config.dump_ast) {
    std::print("{}\n", compound.join('\n'));
  }

  // exit(1);
  // ast::validator::validate(compound);
  auto& cunit = ir::compilation_unit::instance(&compound, src);

  fs::ofile asm_file(m_output_filename);
  asm_file = asm_file.replace_extension("asm");

  try {
    auto asm_code = cunit.compile(compound);
    asm_file.write(asm_code);
    return asm_file;
  } catch (const compilation_error& e) {
    throw_compilation_error(e.what(), e.loc);
    os::error(e.status);
  }
}

ofile compiler::assemble(ofile& asm_file) {
  // os::execute(std::format("nasm -felf64 -g {}", asm_file.path()));

  if constexpr (!cmm::config::assembly) {
    // asm_file.remove();
  }
  return asm_file.replace_extension("o");
}

ofile compiler::link(ofile& obj_file) {
  auto binary = obj_file.replace_extension("");
  // os::execute(std::format("ld -o {} {}", binary.path(), obj_file.path()));
  obj_file.remove();
  return binary;
}

fs::ofile compiler::run() {
  LOG_PATH();
  spdlog::info("Compiling {}", m_source_code.get_filename());

  preprocess();
  auto asm_file     = compile(m_source_code);

  auto obj_file     = assemble(asm_file);
  const auto binary = link(obj_file);

  spdlog::info("Succesfully compiled {} into {}",
                m_source_code.get_filename(),
                binary.path().string());

  if (m_config.dump_memory) {
    // memory::Allocator::get().report_statistics();
  }
  return binary;
}

void compiler::throw_linking_error(const std::string& err) {
  spdlog::error("{}\n", err);
  cmm::os::error(os::status::LINKING_ERROR);
}

void compiler::throw_compilation_error(std::string_view err,
                                       const location& loc) {
  auto formatted_msg =
      strings::generate_compilation_message(m_source_code, err, loc);

  std::cout << formatted_msg;
}
} // namespace cmm
