#include "../include/compiler.hpp"
#include "cxxopts.hpp"
#include "messages.hpp"
#include <algorithm>
#include <csignal>
#include <cstdlib>
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
          compiler::DEFAULT_OUTPUT_FILENAME))(
      ADD_DUMP_OPT("dump-ast", "Dump ast nodes"))(
      ADD_DUMP_OPT("dump-memory", "Dump memory statistics"))(
      ADD_DUMP_OPT("dump-state", "Dump state"))(
      "dump-tokens", "Dump tokens", cxxopts::value<bool>());

  options.parse_positional(INPUT_ARG);
  auto result        = options.parse(argc, argv);

  auto* default_file = std::getenv("CMM_CURRENT_FILE");

  if (result.count("help") > 0) {
    std::cout << options.help() << '\n';
    return 0;
  }

  if (result[INPUT_ARG].count() > 0) {
  } else if (default_file != nullptr) {
    input_file.emplace(default_file);
  }

  if (!input_file.has_value()) {
    spdlog::error("No input file providad");
    exit(EXIT_FAILURE);
  }

  fs::path input_path{input_file.value()};

  if (!fs::exists(input_path)) {
    spdlog::error("Input file {} does not exist", input_path);
    exit(EXIT_FAILURE);
  }

  bool dump_tokens = GET_DUMP_OPT("dump-tokens");
  bool dump_ast    = GET_DUMP_OPT("dump-ast");
  bool dump_state  = GET_DUMP_OPT("dump-state");
  bool dump_memory = GET_DUMP_OPT("dump-memory");

  config conf{dump_tokens, dump_ast, dump_state, dump_memory};

  compiler_instance.run();

  return EXIT_SUCCESS;
}
