#include "common.hpp"
#include "compiler.hpp"
#include <cstdlib>
#include <cxxopts.hpp>
#include <execinfo.h>
#include <iostream>
#include <unistd.h>

#define ADD_DUMP_OPT(OPTION, DESC) OPTION, DESC, cxxopts::value<bool>()
#define GET_DUMP_OPT(OPTION)       result[OPTION].as<bool>()

using namespace cmm;

int main(int argc, char* argv[]) {
  constexpr static std::string INPUT_ARG = "input";

  cxxopts::Options options("CmmLang", "Compiler for cmm language");
  options.add_options()("h,help", "Show help")(
      "o,out",
      "Compiled file path",
      cxxopts::value<std::string>()->default_value(
          compiler::DEFAULT_OUTPUT_FILENAME))(
      ADD_DUMP_OPT("dump-ast", "Dump ast nodes"))(
      ADD_DUMP_OPT("dump-memory", "Dump memory statistics"))(
      ADD_DUMP_OPT("dump-state", "Dump state"))(
      "dump-tokens", "Dump tokens", cxxopts::value<bool>());

  options.add_options("Positional")(
      INPUT_ARG, "Input file", cxxopts::value<std::string>());
  options.parse_positional({INPUT_ARG});
  auto result        = options.parse(argc, argv);

  auto* default_file = std::getenv("CMM_CURRENT_FILE");

  if (result["help"].count() > 0) {
    std::cout << options.help() << '\n';
    return 0;
  }

  std::optional<std::string> input_file;

  if (result[INPUT_ARG].count() > 0) {
    input_file = result[INPUT_ARG].as<std::string>();
  } else if (default_file != nullptr) {
    input_file.emplace(default_file);
  }

  if (!input_file.has_value()) {
    REGISTER_ERROR("No input file providad");
    exit(EXIT_FAILURE);
  }

  fs::path input_path{input_file.value()};

  if (!fs::exists(input_path)) {
    REGISTER_ERROR("Input file {} does not exist", input_path.string());
    exit(EXIT_FAILURE);
  }

  bool dump_tokens = GET_DUMP_OPT("dump-tokens");
  bool dump_ast    = GET_DUMP_OPT("dump-ast");
  bool dump_state  = GET_DUMP_OPT("dump-state");
  bool dump_memory = GET_DUMP_OPT("dump-memory");

  config conf{dump_tokens, dump_ast, dump_state, dump_memory};

  compiler compiler_instance{
      conf,
      input_path,
  };
  compiler_instance.run();

  return EXIT_SUCCESS;
}
