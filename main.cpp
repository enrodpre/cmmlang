#include <cstdlib>
#include <cxxopts.hpp>
#include <iostream>
#include <memory>
#include <optional>
#include <spdlog/spdlog.h>
#include <string>

#include "compiler.hpp"
#include "os.hpp"

using namespace cmm;

// static uint64_t s_allocs = 0;
// static uint64_t s_size   = 0;
// void* operator new(size_t t_size) {
//   s_allocs++;
//   s_size += t_size;
//   std::cout << "Allocating " << t_size << '\n';
//   return malloc(t_size);
// }
//
// void print_allocations() {
//   std::cout << "Allocated " << s_size << " bytes " << s_allocs << "times" << '\n';
// }

int main(int argc, char* argv[]) {
  // std::atexit(print_allocations);
  std::string INPUT_ARG               = "input";
  std::string DEFAULT_OUTPUT_FILENAME = "res";
  logging::initialize_logging();

  cxxopts::Options options("CmmLang", "Compiler for cmm language");
  options.add_options()("h,help", "Show help")(
      "o,out", "Compiled file path", cxxopts::value<std::string>())("dump-ast", "Dump ast nodes")(
      "dump-memory", "Dump memory statistics")("dump-state", "Dump state")(
      "dump-tokens", "Dump tokens")("a,asm", "Don't remove intermediate generated asm");

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
    REGISTER_ERROR("No input file provided");
    return os::status_code::INVALID_ARGS;
  }

  std::filesystem::path input_path{input_file.value()};

  if (!std::filesystem::exists(input_path)) {
    REGISTER_ERROR("Input file {} does not exist", input_path.string());
    return os::status_code::FILE_NOT_FOUND;
  }

  return compiler::compile(input_path, output_name);
}
