#pragma once

#include <cstdint>
#include <type_traits>
#include <vector>

#include "ast/tree.hpp"
#include "macros.hpp"
#include "memory.hpp"
#include "token.hpp"

namespace cmm::ast::expr {
enum class literal_t : uint8_t;
struct binary_operator;
struct call;
struct expression;
struct identifier;
struct literal;
struct unary_operator;
} // namespace cmm::ast::expr

namespace cmm::parser {

class parser {

public:
  parser(tokens);
  ~parser() = default;
  NOT_MOVABLE_CLS(parser);
  NOT_COPYABLE_CLS(parser);

  ast::translation_unit* parse();
  ast::translation_unit* parse_program();
  ast::statement* parse_statement();
  template <typename T = ast::decl::block>
    requires(std::is_same_v<ast::decl::function::definition, T> ||
             std::is_same_v<ast::decl::block, T>)
  T* parse_block();
  ast::statement* parse_if();
  ast::statement* parse_goto();
  ast::statement* parse_label();
  ast::statement* parse_while();
  ast::statement* parse_for();
  ast::declaration* parse_declaration();

  // EXPRESSIONS
  ast::expr::expression& parse_primary();
  ast::expr::expression& parse_condition();
  ast::expr::expression& parse_unary_expr();
  ast::expr::expression& parse_call(const token&);
  ast::expr::expression& parse_expr(uint8_t = 255);
  ast::expr::expression& parse_lhs_expr();

  // Terms
  ast::decl::specifiers parse_specifiers();
  ast::decl::rank* parse_rank();

private:
  tokens m_tokens;
  memory::arena& m_arena;
  std::vector<ast::declaration*> m_global;
  ast::translation_unit* m_pointer;

  // Helpers
  template <typename Func, typename T = std::invoke_result_t<Func>>
  ast::siblings<T> parse_varargs(Func&&, const token_t&, const token_t&, const token_t&);
  ast::decl::variable* parse_variable(ast::decl::specifiers&&, const token&);
  ast::decl::function* parse_function(ast::decl::specifiers&&, const token&);
  template <typename T, typename... Args>
  T* create_node(Args&&...);

  // Checkers
  [[nodiscard]] token_data peek_data() const;
  [[nodiscard]] bool next_is(token_t) const;
  void want_semicolon();
  struct semicolon_consumer {
    parser& p;
    semicolon_consumer(parser& pa)
        : p(pa) {}
    ~semicolon_consumer() { p.want_semicolon(); }
  };
  semicolon_consumer consume_semicolon() { return {*this}; }
};

} // namespace cmm::parser
