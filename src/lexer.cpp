#include "lexer.hpp"

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

bool lexer::has_next() const {
  return m_pointer < m_src.size();
}

char lexer::peek() const {
  return m_src[m_pointer];
}

cmm::cstring lexer::peek(size_t size) const {
  return std::string_view(m_src).substr(m_pointer, size);
}

cmm::cstring lexer::peek_remaining() const {
  return std::string_view(m_src).substr(m_pointer);
}

void lexer::advance(const size_t size = 1) {
  m_pointer += size;
  m_column += size;
}

tokens lexer::tokenize() {
  // Trying to aproximate number of tokens
  auto approximated_number_tokens =
      std::count(m_src.cbegin(), m_src.cend(), ' ');
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
  return token_t::properties_array() | std::views::filter([](auto pair) {
           const auto& [type, prop] = pair;
           return prop.pattern_type ==
                  token_t::properties::pattern_t::SINGLE_CHAR;
         });
}

auto constexpr lexer::multi_patterns() {
  return cmm::token_t::properties_array() | std::views::filter([](auto pair) {
           const auto& [type, prop] = pair;
           return prop.pattern_type ==
                  token_t::properties::pattern_t::MULTI_CHAR;
         });
}

auto constexpr lexer::regex_patterns() {
  using pair_           = std::pair<token_t, std::string>;
  auto compare_patterns = [](const pair_& a, const pair_& b) {
    return a.second > b.second;
  };

  auto sorted =
      cmm::token_t::properties_array() |
      std::views::filter([](const auto& pair) {
        const auto& [type, prop] = pair;
        return prop.pattern_type == token_t::properties::pattern_t::REGEX;
      }) |
      std::views::transform([](const auto& pair) {
        const auto& [type, prop] = pair;
        return std::make_pair<token_t, std::string>(type,
                                                    std::string(prop.pattern));
      }) |
      std::ranges::to<std::vector>();
  std::ranges::sort(sorted, compare_patterns);
  return sorted;
}

void lexer::create_token(cmm::token_t&& type, size_t length) {
  m_tokens.emplace_back(std::move(type), location(m_row, 1, m_column, length));
  advance(length);
}
void lexer::create_token(cmm::token_t&& type, cmm::cstring value) {
  advance(value.length());

  m_tokens.emplace_back(
      std::move(type), location(m_row, 1, m_column, value.length()), value);
}

void lexer::parse_token() {
  // Debug tokens if :TOKENS
  if (peek(7) == ":TOKENS") {
    spdlog::debug("{}", m_tokens.join(" "));
  }

  // Try first reserved words
  for (auto const& [tokenType, reserved] : multi_patterns()) {
    auto pattern       = reserved.pattern;
    size_t word_length = pattern.length();
    // spdlog::trace("Trying to match {} with {}", peek(word_length),
    // pattern.pattern)
    if (peek(word_length) == pattern) {
      create_token(tokenType, word_length);
      return;
    }
  }

  // Then try monotokens
  for (auto const& [tokenType, reserved] : single_patterns()) {
    if (reserved.pattern.find(peek()) != std::string::npos) {
      create_token(tokenType, 1);
      return;
    }
  }

  // Try patterns otherwise, as func or var identifiers
  for (auto& [tokenType, pattern] : regex_patterns()) {
    std::cmatch match;
    std::regex re("^" + pattern);
    auto next_token = peek_remaining();
    /* spdlog::trace("Trying {}", tokenType); */
    if (std::regex_search(next_token.cbegin(), next_token.cend(), match, re)) {
      const auto* start =
          m_src.cbegin() + (cmm::cstring::difference_type)m_pointer;
      const auto* end = start + match[1].length();
      cmm::cstring sv{start, end};

      create_token(std::move(tokenType), sv);
      // Consume ditched matched chars
      // as colon in label
      advance(match[0].length() - match[1].length());
      return;
    }
  }

  // If nothing was matched
  spdlog::error("Error reading token {}", peek_remaining());
  exit(1);
}
} // namespace cmm
