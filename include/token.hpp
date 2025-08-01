#pragma once

#include "common.hpp"
#include "types.hpp"
#include <cstdint>
#include <initializer_list>
#include <ranges>
#include <unordered_map>
#include <utility>

namespace cmm {

enum class _token_t : uint8_t {
  // specifiers
  types_begin,
  void_t,
  nullptr_t,
  bool_t,
  char_t,
  byte_t,
  int_t,
  short_t,
  double_t,
  long_t,
  float_t,
  class_t,
  enum_t,
  struct_t,
  types_end,
  /* auto_t, */

  // Storage
  storage_begin,
  static_,
  extern_,
  mutable_,
  register_,
  storage_end,

  modifier_begin,
  friend_,
  constexpr_,
  const_,
  volatile_,
  ptr,
  ref,
  signed_,
  unsigned_,
  constinit_,
  consteval_,
  modifier_end,

  operator_begin,
  dot,
  arrow,
  and_,
  or_,
  assign,
  add_a,
  sub_a,
  mul_a,
  div_a,
  eq,
  neq,
  lt,
  le,
  gt,
  ge,
  plus,
  minus,
  star,
  fslash,
  expr_end,
  ampersand,
  not_,
  inc,
  dec,
  bslash,
  comma,
  operator_end,

  // Pairs
  o_paren,
  c_paren,
  o_curly,
  c_curly,
  o_bracket,
  c_bracket,
  expr,
  s_quotes,
  d_quotes,
  o_attr,
  c_attr,

  // literal
  literal_begin,
  bool_lit,
  int_lit,
  float_lit,
  str_lit,
  char_lit,
  literal_end,
  ident,
  label,
  operator_,

  // symbols (5 bits (20-24))
  dbl_colon,

  // Statemens
  if_,
  else_,
  while_,
  for_,
  break_,
  continue_,
  return_,
  goto_,
  semicolon,
  total,

  // Compiler
  debug_ast,
  debug_mem,
  debug_state,
  debug_tokens
};
#define IS_GROUP(FUNCNAME, NAME) \
  [[nodiscard]] bool FUNCNAME() const { \
    return CONCAT(NAME, _begin) < m_value && m_value < CONCAT(NAME, _end); \
  }
struct token_t : public cmm::enumeration<_token_t> {
  struct properties {
    enum class pattern_t : uint8_t { SINGLE_CHAR, MULTI_CHAR, REGEX };
    const pattern_t pattern_type;
    const cstring pattern;
    std::optional<type_t> type;

    constexpr properties(pattern_t, cstring, type_t = {}) noexcept;
  };

  using value_type = _token_t;
  using enumeration<value_type>::enumeration;
  using enum value_type;
  using properties_map = std::unordered_map<value_type, properties>;
  [[nodiscard]] static constexpr const properties_map& properties_array();
  [[nodiscard]] constexpr const properties& get_properties() const {
    return properties_array().at(m_value);
  };

  [[nodiscard]] bool is(const token_t& t) const { return m_value == t.m_value; }

  IS_GROUP(is_type, types)
  IS_GROUP(is_operator, operator)
  IS_GROUP(is_storage, storage);
  IS_GROUP(is_modifier, modifier);
  IS_GROUP(is_literal, literal);

  [[nodiscard]] bool is_specifier() const { return is_storage() || is_modifier() || is_type(); }

  [[nodiscard]] bool is_from_expression() const {
    return is_literal() || is(ident) || is_operator() || is(o_paren) || is(c_paren);
  }
};

struct token : public formattable, public self_allocated {
  token_t type;
  std::string value;

  token(token_t&& t, cmm::location&& loc)
      : self_allocated(std::move(loc)),
        type(std::move(t)) {}
  token(token_t&& t, cmm::location&& loc, cmm::cstring value)
      : self_allocated(std::move(loc)),
        type(std::move(t)),
        value(value) {}
  bool operator==(const token& other) const { return value == other.value && type == other.type; }
  [[nodiscard]] std::string format() const override;
};

class tokens : public formattable_range<std::vector<token>> {
  using range_type = std::vector<token>;

private:
  using RCtoken        = const token&;
  using const_iterator = std::vector<token>::const_iterator;

public:
  tokens();
  tokens(const std::vector<token>&);
  tokens(std::initializer_list<token>);

  [[nodiscard]] const std::vector<token>& data() const noexcept;
  [[nodiscard]] size_t size() const noexcept;
  [[nodiscard]] bool empty() const noexcept;
  [[nodiscard]] bool has_next() const noexcept;
  [[nodiscard]] bool next_is(const token_t&) const noexcept;
  [[nodiscard]] RCtoken next();
  void advance(size_t = 1);
  RCtoken peek(short = 0);
  void pointer(size_t) noexcept;
  [[nodiscard]] size_t pointer() const noexcept;
  void reserve(size_t);
  void emplace(cmm::token_t&&, location&&, cstring = "");

  [[nodiscard]] const_iterator cbegin() const noexcept;
  [[nodiscard]] const_iterator cend() const noexcept;

  template <typename Range, typename Func>
  Range take_while(Func&& func);

  // Helpers
  [[nodiscard]] std::string format() const;

private:
  std::vector<token> m_tokens;
  size_t m_pointer;
};

template <typename Range, typename Func>
Range tokens::take_while(Func&& func) {
  auto range = m_tokens | std::ranges::take_while_view(func);
  advance(std::ranges::size(range));
  return range;
}
} // namespace cmm

#include "token.inl"
