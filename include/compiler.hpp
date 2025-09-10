#pragma once

#include <cstddef>
#include <filesystem>
#include <iterator>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

#include "ast.hpp"
#include "common.hpp"
#include "fs.hpp"

namespace cmm {
struct location;

class source_code {
public:
  struct iterator {
    using value_type        = std::string_view;
    using difference_type   = std::ptrdiff_t;
    using iterator_category = std::forward_iterator_tag;
    using pointer           = value_type*;
    using reference         = value_type;

    iterator()              = default;
    iterator(std::string_view t_code)
        : m_code(t_code) {}
    reference operator*() const { return m_current; }
    // iterator& operator++() {}
    // iterator& operator++(int) {}

    bool operator==(const iterator& other) const {
      return m_code.data() == other.m_code.data() && m_code.size() == other.m_code.size();
    }

  private:
    std::string_view m_code;
    std::string_view m_current;
    constexpr static inline auto s_delim                = ' ';
    constexpr static inline auto s_string_literal_regex = "\"(.*)\"";
    constexpr static inline auto s_char_literal_regex   = "'(.)'";

    // void find_next() {
    //   m_current.find_first_not_of(' ')
    // }
  };

  source_code(const fs::ifile&);

  [[nodiscard]] std::string_view get_code() const;
  [[nodiscard]] std::string get_filename() const;
  [[nodiscard]] std::string_view get_nth_line(size_t) const;
  [[nodiscard]] std::tuple<std::string_view, std::string_view, std::string_view> get_line_chunked(
      const location&) const;
  std::tuple<size_t, size_t, size_t> to_coordinates(const cmm::location&) const;

  std::vector<std::string> build_compilation_error(
      const std::vector<const ast::decl::function::definition*>&,
      std::string_view,
      const location&);

private:
  std::string m_code;
  std::filesystem::path m_file;

  std::string build_full_location(const location&);
};

class compiler {

public:
  compiler(const fs::path&, const std::string&);
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

  static void throw_linking_error(const std::string&);

private:
  source_code m_source_code;
  std::string_view m_output_filename;
};

}; // namespace cmm
