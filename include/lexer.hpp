#pragma once

#include "token.hpp"

namespace cmm {
class lexer {
public:
  explicit lexer(cmm::cstring);
  lexer(const lexer&)            = delete;
  lexer(lexer&&)                 = delete;
  lexer& operator=(const lexer&) = delete;
  lexer& operator=(lexer&&)      = delete;
  ~lexer()                       = default;
  [[nodiscard]] tokens tokenize();

private:
  std::vector<token> m_tokens;
  cmm::cstring m_src;
  size_t m_pointer;

  [[nodiscard]] bool has_next() const;
  [[nodiscard]] cstring peek(int) const;
  [[nodiscard]] cstring peek(size_t) const;
  [[nodiscard]] char peek() const;
  void advance(size_t);
  void parse_token();
  [[nodiscard]] static auto constexpr single_patterns();
  [[nodiscard]] static auto constexpr multi_patterns();
  [[nodiscard]] static auto constexpr regex_patterns();
};
}; // namespace cmm
