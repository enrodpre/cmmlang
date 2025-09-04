#pragma once

#include <stddef.h>
#include <vector>

#include "token.hpp"

namespace cmm {
class lexer {
public:
  explicit lexer(std::string_view);
  lexer(const lexer&)            = delete;
  lexer(lexer&&)                 = delete;
  lexer& operator=(const lexer&) = delete;
  lexer& operator=(lexer&&)      = delete;
  ~lexer()                       = default;
  [[nodiscard]] tokens tokenize();

private:
  std::vector<token> m_tokens;
  std::string_view m_src;
  size_t m_pointer;

  [[nodiscard]] bool has_next() const;
  [[nodiscard]] std::string_view peek(int) const;
  [[nodiscard]] std::string_view peek(size_t) const;
  [[nodiscard]] char peek() const;
  void advance(size_t);
  void parse_token();
  [[nodiscard]] static auto constexpr single_patterns();
  [[nodiscard]] static auto constexpr multi_patterns();
  [[nodiscard]] static auto constexpr regex_patterns();
};
}; // namespace cmm
