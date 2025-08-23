#include <cstdlib>
#include <cxxopts.hpp>
#include <iostream>
#include <memory>
#include <optional>
#include <string>

#include "common.hpp"
#include "compiler.hpp"
#include "fs.hpp"

#define ADD_DUMP_OPT(OPTION, DESC) OPTION, DESC, cxxopts::value<bool>()
#define GET_DUMP_OPT(OPTION)       result[OPTION].as<bool>()

using namespace cmm;

int main(int argc, char* argv[]) {
  std::string INPUT_ARG               = "input";
  std::string DEFAULT_OUTPUT_FILENAME = "res";

  cxxopts::Options options("CmmLang", "Compiler for cmm language");
  options.add_options()("h,help", "Show help")(
      "o,out", "Compiled file path", cxxopts::value<std::string>())("dump-ast", "Dump ast nodes")(
      "dump-memory", "Dump memory statistics")("dump-state", "Dump state")("dump-tokens",
                                                                           "Dump tokens");

  options.add_options("Positional")(INPUT_ARG, "Input file", cxxopts::value<std::string>());
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
  std::string output_name = DEFAULT_OUTPUT_FILENAME;
  if (result.contains("out")) {
    output_name = result["out"].as<std::string>();
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

  // bool dump_tokens = GET_DUMP_OPT("dump-tokens");
  // bool dump_ast    = GET_DUMP_OPT("dump-ast");
  // bool dump_state  = GET_DUMP_OPT("dump-state");
  // bool dump_memory = GET_DUMP_OPT("dump-memory");

  compiler compiler_instance{input_path, output_name};
  compiler_instance.run();

  return EXIT_SUCCESS;
}
