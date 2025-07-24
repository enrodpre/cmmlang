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
  [[nodiscard]] const char* what() const noexcept override {
    return message.c_str();
  }
};

class no_token_matched_exception : public parser_exception {
  ast::compound statements;

public:
  no_token_matched_exception(const token& token, decltype(statements) stmts)

      : parser_exception(
            std::format("No token has been matched.\n Current token: {}\n",
                        token)),
        statements(std::move(stmts)) {}
};

class parser {

public:
  parser(tokens);
  ~parser()                        = default;
  parser(const parser&)            = delete;
  parser(parser&&)                 = delete;
  parser& operator=(const parser&) = delete;
  parser& operator=(parser&&)      = delete;

  ast::program parse();
  ast::program parse_program();
  ast::statement* parse_statement();
  ast::compound& parse_compound();
  ast::statement* parse_if();
  ast::statement* parse_goto();
  ast::statement* parse_label();
  ast::statement* parse_while();
  ast::statement* parse_for();
  ast::decl::global_declaration& parse_declaration();
  ast::decl::variable& parse_variable(ast::decl::specifiers&&,
                                      const ast::term::identifier*);
  ast::decl::function& parse_function(ast::decl::specifiers&&,
                                      const ast::term::identifier&);

  // EXPRESSIONS
  ast::expr::expression* parse_primary();
  ast::expr::expression* parse_condition();
  ast::expr::expression* parse_unary_expr();
  ast::expr::expression* parse_call(const ast::term::identifier& ident);
  ast::expr::expression* parse_expr(uint8_t = 0);

  // Terms
  ast::decl::specifiers parse_specifiers();
  template <bool Optional = true>
  ast::term::identifier* parse_identifier();

private:
  tokens m_tokens;
  memory::Allocator m_arena;
  ast::program m_global;
  cmm::stack<ast::compound> m_compound;

  ast::expr::expression* parse_lhs_expr();
  void store_statement(ast::statement*);
  static void want(const token&, const token_t&, bool = false);
  void want(const token_t&, bool = false);
  bool need_semicolon_after_statement = true;
  void want_semicolon();
};

} // namespace cmm::parser
