#include "token.hpp"
#include "common.hpp"

#include <cstddef>

namespace cmm {
using pattern_t = token_t::properties::pattern_t;

using RCtoken   = const token&;

std::string token::format() const {
  if (!value.empty()) {
    return std::format("token:\n  {}\n  location: {}\n  Value: {}\n", type, location(), value);
  }

  return std::format("token:\n  {}\n  location: {}\n", type, location());
}
tokens::tokens()
    : formattable_range(&m_tokens),
      m_pointer() {}

tokens::tokens(const std::vector<token>& toks)
    : m_tokens(toks),
      formattable_range(&m_tokens),
      m_pointer(0) {}

tokens::tokens(std::initializer_list<token> list)
    : formattable_range(&m_tokens),
      m_tokens(list),
      m_pointer() {}

size_t tokens::size() const noexcept { return m_tokens.size(); }

[[nodiscard]] const std::vector<token>& tokens::data() const noexcept { return m_tokens; }

bool tokens::empty() const noexcept { return size() == 0; }

bool tokens::has_next() const noexcept { return pointer() < size(); }

[[nodiscard]] bool tokens::next_is(const token_t& other) const noexcept {
  return has_next() && m_tokens.at(m_pointer).type.is(other);
}

RCtoken tokens::next() {
  const auto& t = peek(0);
  advance();
  return t;
}

void tokens::advance(size_t n) { m_pointer += n; }

RCtoken tokens::peek(short offset) { return m_tokens.at(m_pointer + offset); }

size_t tokens::pointer() const noexcept { return m_pointer; }

tokens::const_iterator tokens::cbegin() const noexcept { return m_tokens.cbegin(); }
tokens::const_iterator tokens::cend() const noexcept { return m_tokens.cend(); }

[[nodiscard]] std::string tokens::format() const {
  return std::format("tokens({})", m_tokens.size());
}
void tokens::reserve(size_t n) { m_tokens.reserve(n); }

} // namespace cmm
