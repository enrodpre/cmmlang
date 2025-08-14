#pragma once

#include "allocator.hpp"
#include "ast.hpp"
#include <format>

#include "token.hpp"
#include <utility>

namespace cmm::parser {
class parser_exception : public std::exception {
protected:
  std::string message;

public:
  static constexpr std::string_view TEMPLATE = "parser Exception.\n{}. {}";
  parser_exception(const std::string& data)
      : message(std::format(TEMPLATE, data, libassert::stacktrace())) {}
  [[nodiscard]] const char* what() const noexcept override { return message.c_str(); }
};

class no_token_matched_exception : public parser_exception {
  std::vector<ast::statement*> statements;

public:
  no_token_matched_exception(const token& token, decltype(statements) stmts)

      : parser_exception(std::format("No token has been matched.\n Current token: {}\n", token)),
        statements(std::move(stmts)) {}
};

class parser {

public:
  parser(tokens);
  ~parser() = default;
  NOT_MOVABLE_CLS(parser);
  NOT_COPYABLE_CLS(parser);

  ast::translation_unit parse();
  ast::translation_unit parse_program();
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
  ast::expr::expression* parse_primary();
  ast::expr::expression* parse_condition();
  ast::expr::expression* parse_unary_expr();
  ast::expr::expression* parse_call(ast::identifier&& ident);
  ast::expr::expression* parse_expr(uint8_t = 255);

  // Terms
  ast::decl::specifiers parse_specifiers();
  ast::identifier parse_identifier();
  ast::decl::rank* parse_rank();

private:
  tokens m_tokens;
  memory::Allocator m_arena;
  std::vector<ast::declaration*> m_global;

  // Helpers
  template <typename T, typename Func>
  ast::siblings<T> parse_varargs(Func&&, const token_t&, const token_t&, const token_t&);
  ast::decl::variable* parse_variable(ast::decl::specifiers&&, ast::identifier&&);
  ast::decl::function* parse_function(ast::decl::specifiers&&, ast::identifier&&);
  ast::expr::expression* parse_lhs_expr();
  template <typename T, typename... Args>
    requires(std::is_constructible_v<T, Args...>)
  T* create_node(Args&&...);

  // Checkers
  [[nodiscard]] token_data peek_data() const;
  [[nodiscard]] bool next_is(token_t) const;
  static void want(const token&, const token_t&, bool = false);
  token want(const token_t&, bool = false);
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
