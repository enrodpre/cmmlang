#include "lexer.hpp"

#include <algorithm>
#include <ranges>
#include <regex>
#include <string>
#include <string_view>

using cmm::token_t;
namespace cmm {

lexer::lexer(cmm::cstring src)
    : m_src(src),
      m_pointer(0),
      m_row(1),
      m_column(1) {};

bool lexer::has_next() const { return m_pointer < m_src.size(); }

char lexer::peek() const { return m_src[m_pointer]; }

cmm::cstring lexer::peek(size_t size) const { return m_src.substr(m_pointer, size); }

cmm::cstring lexer::peek(int size) const {
  if (size < 0) {
    return m_src.substr(m_pointer);
  }
  return peek(static_cast<size_t>(size));
}

void lexer::advance(const size_t size = 1) {
  m_pointer += size;
  m_column += size;
}

tokens lexer::tokenize() {
  // Trying to aproximate number of tokens
  auto approximated_number_tokens = std::count(m_src.cbegin(), m_src.cend(), ' ');
  m_tokens.reserve(approximated_number_tokens);

  while (has_next()) {
    if (isspace(peek()) != 0) {
      if (peek() == '\n') {
        m_column = 0;
        m_row += 1;
      }
      advance();
    } else {
      parse_token();
    }
  }

  return m_tokens;
}

auto constexpr lexer::single_patterns() {
  return token_t::properties_array() | std::views::filter([](const auto& pair) {
           const auto& [type, prop] = pair;
           return prop.pattern_type == token_t::properties::pattern_t::SINGLE_CHAR;
         });
}

auto constexpr lexer::multi_patterns() {
  return cmm::token_t::properties_array() | std::views::filter([](const auto& pair) {
           const auto& [type, prop] = pair;
           return prop.pattern_type == token_t::properties::pattern_t::MULTI_CHAR;
         });
}

auto constexpr lexer::regex_patterns() {
  using pair_           = std::pair<token_t, std::string>;
  auto compare_patterns = [](const pair_& a, const pair_& b) { return a.second > b.second; };

  auto sorted = cmm::token_t::properties_array() | std::views::filter([](const auto& pair) {
                  const auto& [type, prop] = pair;
                  return prop.pattern_type == token_t::properties::pattern_t::REGEX;
                }) |
                std::views::transform([](const auto& pair) {
                  const auto& [type, prop] = pair;
                  return std::make_pair<token_t, std::string>(type, std::string(prop.pattern));
                }) |
                std::ranges::to<std::vector>();
  std::ranges::sort(sorted, compare_patterns);
  return sorted;
}

void lexer::parse_token() {
  if (peek(7) == ":TOKENS") {
    // REGISTER_DEBUG("{}", m_tokens.join(" "));
  }

  // Try first reserved words
  for (auto const& [tokenType, reserved] : multi_patterns()) {
    auto pattern       = reserved.pattern;
    size_t word_length = pattern.length();
    if (peek(word_length) == pattern) {
      advance(word_length);
      m_tokens.emplace_back(tokenType, location(m_row, 1, m_column, word_length));
      return;
    }
  }

  // Try patterns otherwise, as func or var identifiers
  for (auto& [tokenType, pattern] : regex_patterns()) {
    std::cmatch match;
    std::regex re("^" + pattern);
    auto next_token = peek(-1);
    /* REGISTER_TRACE("Trying {}", tokenType); */
    if (std::regex_search(next_token.cbegin(), next_token.cend(), match, re)) {
      m_tokens.emplace_back(
          std::move(tokenType), location(m_row, 1, m_column, match[0].length()), match[1].str());
      // Consume ditched matched chars
      // as colon in label
      advance(match[0].length());
      return;
    }
  }

  // Then try monotokens
  for (auto const& [tokenType, reserved] : single_patterns()) {
    if (reserved.pattern.find(peek()) != std::string::npos) {
      m_tokens.emplace_back(tokenType, location(m_row, 1, m_column, 1));
      advance();
      return;
    }
  }

  // If nothing was matched
  REGISTER_ERROR("Error reading token {}", peek(-1));
  exit(1);
}
} // namespace cmm
