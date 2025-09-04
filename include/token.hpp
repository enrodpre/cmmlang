#pragma once

#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <magic_enum/magic_enum.hpp>
#include <magic_enum/magic_enum_containers.hpp>
#include <ranges>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

#include "common.hpp"
#include "macros.hpp"

namespace cmm {
struct token_data;

enum class token_t : uint8_t {
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
  binary_begin,
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
  fslash,
  unary_begin,
  plus,
  minus,
  star,
  ampersand,
  expr_end,
  binary_end,
  not_,
  inc,
  dec,
  unary_end,
  bslash,
  operator_end,
  comma,

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
  false_lit,
  true_lit,
  int_lit,
  float_lit,
  string_lit,
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
#define IS_GROUP(FUNCNAME, NAME)                                           \
  [[nodiscard]] bool FUNCNAME() const {                                    \
    return CONCAT(NAME, _begin) < m_value && m_value < CONCAT(NAME, _end); \
  }
struct token_data : public cmm::enumeration<token_t> {
  enum class pattern_t : uint8_t { SINGLE_CHAR, MULTI_CHAR, REGEX };
  const pattern_t pattern_type{};
  const std::string_view pattern;
  using value_type = token_t;
  using enum value_type;
  using enumeration<value_type>::enumeration;
  using member_types   = std::tuple<value_type, pattern_t, std::string_view>;
  using properties_map = magic_enum::containers::array<value_type, member_types>;
  operator token_t() const { return m_value; }
  constexpr token_data(token_t t)
      : enumeration(t),
        pattern_type(std::get<1>(token_data::properties_array().at(t))),
        pattern(std::get<2>(token_data::properties_array().at(t))) {}
  [[nodiscard]] static constexpr const properties_map& properties_array();

  [[nodiscard]] bool is(const token_t& t) const { return m_value == t; }

  IS_GROUP(is_type, types)
  IS_GROUP(is_storage, storage);
  IS_GROUP(is_modifier, modifier);
  IS_GROUP(is_literal, literal);
  IS_GROUP(is_operator, operator)
  IS_GROUP(is_unary_operator, unary)
  IS_GROUP(is_binary_operator, binary)

  [[nodiscard]] bool is_specifier() const { return is_storage() || is_modifier() || is_type(); }

  [[nodiscard]] bool is_from_expression() const {
    return is_literal() || is(ident) || is_operator() || is(o_paren) || is(c_paren);
  }
};

struct token : public displayable {
  token_t type;
  std::string_view value;
  token(token_t t, cmm::location&& t_location, std::string_view t_sv = {})
      : type(t),
        value(t_sv),
        m_location(std::move(t_location)) {}
  bool operator==(const token& other) const { return value == other.value && type == other.type; }
  const cmm::location& location() const { return m_location; }
  [[nodiscard]] std::string string() const override;

private:
  cmm::location m_location;
};

class tokens : public formattable_range<std::vector<token>> {
  using range_type = std::vector<token>;

private:
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
  token next();
  void advance(size_t = 1);
  [[nodiscard]] const token& peek(short = 0) const;
  void pointer(size_t) noexcept;
  [[nodiscard]] size_t pointer() const noexcept;
  void reserve(size_t);
  void emplace(cmm::token_t&&, location&&, std::string_view = "");

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
