#include "lexer.hpp"

#include <algorithm>
#include <ranges>
#include <regex>
#include <string>
#include <string_view>

namespace cmm {

lexer::lexer(cmm::cstring src)
    : m_src(src),
      m_pointer(0) {};

bool lexer::has_next() const { return m_pointer < m_src.size(); }

char lexer::peek() const { return m_src[m_pointer]; }

cmm::cstring lexer::peek(size_t size) const { return m_src.substr(m_pointer, size); }

cmm::cstring lexer::peek(int size) const {
  if (size < 0) {
    return m_src.substr(m_pointer);
  }
  return peek(static_cast<size_t>(size));
}

void lexer::advance(const size_t size = 1) { m_pointer += size; }

tokens lexer::tokenize() {
  // Trying to aproximate number of tokens
  auto approximated_number_tokens = std::count(m_src.cbegin(), m_src.cend(), ' ');
  m_tokens.reserve(approximated_number_tokens);

  while (has_next()) {
    if (isspace(peek()) != 0) {
      if (peek() == '\n') {}
      advance();
    } else {
      parse_token();
    }
  }

  return m_tokens;
}

auto constexpr lexer::single_patterns() {
  return std::views::filter(token_data::properties_array(),
                            [](const auto& props) {
                              return std::get<1>(props) == token_data::pattern_t::SINGLE_CHAR &&
                                     !std::get<2>(props).empty();
                            }) |
         std::views::transform([](const auto& props) {
           return std::make_pair(std::get<0>(props), std::get<2>(props));
         });
  ;
}

auto constexpr lexer::multi_patterns() {
  return std::views::filter(token_data::properties_array(),
                            [](const auto& props) {
                              return std::get<1>(props) == token_data::pattern_t::MULTI_CHAR;
                            }) |
         std::views::transform([](const auto& props) {
           return std::make_pair(std::get<0>(props), std::get<2>(props));
         });
}

auto constexpr lexer::regex_patterns() {

  auto sorted = std::views::filter(token_data::properties_array(),
                                   [](const auto& props) {
                                     return std::get<1>(props) == token_data::pattern_t::REGEX;
                                   }) |
                std::views::transform([](const auto& props) {
                  return std::make_pair(std::get<0>(props), std::get<2>(props));
                }) |
                std::ranges::to<std::vector>();
  std::ranges::sort(sorted, [](const auto& a, const auto& b) { return a.second > b.second; });
  return sorted;
}

void lexer::parse_token() {
  if (peek(7) == ":TOKENS") {
    // REGISTER_DEBUG("{}", m_tokens.join(" "));
  }

  auto multi = multi_patterns() | std::ranges::to<std::vector>();
  // Try first reserved words
  for (auto const& [tokenType, pattern] : multi) {
    size_t word_length = pattern.length();
    if (peek(word_length) == pattern) {
      m_tokens.emplace_back(tokenType, location(m_pointer, m_pointer + word_length));
      advance(word_length);
      return;
    }
  }

  auto regex = regex_patterns() | std::ranges::to<std::vector>();
  // Try patterns otherwise, as func or var identifiers
  for (auto& [tokenType, pattern] : regex) {
    std::cmatch match;
    std::regex re(std::format("^{}", pattern));
    auto next_token = peek(-1);
    if (std::regex_search(next_token.cbegin(), next_token.cend(), match, re)) {
      m_tokens.emplace_back(
          tokenType, location(m_pointer, m_pointer + match[1].str().size()), match[1].str());
      // Consume ditched matched chars
      // as colon in label
      advance(match[0].length());
      return;
    }
  }

  auto single = single_patterns() | std::ranges::to<std::vector>();
  // Then try monotokens
  for (auto const& [tokenType, reserved] : single) {
    if (reserved.find(peek()) != std::string::npos) {
      m_tokens.emplace_back(tokenType, location(m_pointer, m_pointer + 1));
      advance();
      return;
    }
  }

  // If nothing was matched
  REGISTER_ERROR("Error reading token {}", peek(-1));
  exit(1);
}
} // namespace cmm
