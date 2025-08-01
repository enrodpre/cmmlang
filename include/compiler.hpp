#pragma once

#include "common.hpp"
#include "fs.hpp"
#include <cpptrace/cpptrace.hpp>

namespace cmm {

class compiler {

public:
  compiler(const config&, const fs::path&, const std::string&);
  compiler(compiler&&)                 = delete;
  compiler& operator=(compiler&&)      = delete;
  compiler(const compiler&)            = delete;
  compiler& operator=(const compiler&) = delete;
  ~compiler()                          = default;

  static void preprocess(const std::string& = "");
  fs::ofile compile(const source_code&);
  static fs::ofile assemble(fs::ofile& file);
  static fs::ofile link(fs::ofile& file);
  fs::ofile run();

  void throw_compilation_error(std::string_view, const location&);
  static void throw_linking_error(const std::string&);

private:
  source_code m_source_code;
  config m_config;
  cstring m_output_filename;
};

}; // namespace cmm
