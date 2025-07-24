#include "parser.hpp"
#include "allocator.hpp"
#include "ast.hpp"
#include "common.hpp"

#include "lang.hpp"
#include "token.hpp"
#include <magic_enum/magic_enum_format.hpp>
#include <utility>

namespace cmm::parser {

using namespace ast;
// using namespace messaging;

parser::parser(tokens tokens)
    : m_tokens(std::move(tokens)) {
  m_compound.emplace_back();
}

program parser::parser::parse() {
  return parse_program();
}

ast::program parser::parser::parse_program() {
  while (m_tokens.has_next()) {
    m_global.push_back(parse_declaration());
  }
  return m_global;
}

compound& parser::parse_compound() {
  m_compound.emplace_back();
  want(token_t::o_curly);

  while (m_tokens.has_next()) {
    if (m_tokens.peek().type == token_t::c_curly) {
      m_tokens.advance();
      need_semicolon_after_statement = false;
      auto stmts                     = m_compound.pop_return();
      return *m_arena.emplace<compound>(stmts);
    }

    store_statement(parse_statement());
  }

  auto tok = m_tokens.peek();
  throw unexpected_token(tok);

  auto stmts = m_compound.pop_return();
  return *m_arena.emplace<compound>(stmts);
}

statement* parser::parse_statement() {
  token next = m_tokens.peek();
  if (next.type == token_t::debug_ast) {
    // REGISTER_DEBUG("{}", m_compound);
    // return emplace<debug::printobj>(next, debug::Component::ast);
  }
  if (next.type == token_t::debug_mem) {
    // return emplace<debug::printobj>(next, debug::Component::mem);
  }
  if (next.type == token_t::debug_state) {
    // return emplace<debug::printobj>(next, debug::Component::state);
  }
  if (next.type.is_specifier()) {
    // It's a decl
    return &parse_declaration();
  }

  if (next.type.is_operator()) {
    auto* expr = parse_expr();
    want_semicolon();
    return expr;
  }

  switch (next.type.inner()) { // NOLINT

    // If identifier, can be a function call or a var assignment
    case token_t::ident:
      {
        auto* expr = parse_expr();
        want_semicolon();
        return expr;
        break;
      }
    case token_t::if_:
      {
        auto* if_                      = parse_if();
        need_semicolon_after_statement = false;
        return if_;
      }
    case token_t::while_:
      {
        auto* while_                   = parse_while();
        need_semicolon_after_statement = false;
        return while_;
      }
    case token_t::for_:
      {
        auto* for_                     = parse_for();
        need_semicolon_after_statement = false;
        return for_;
      }
    case token_t::continue_:
      {
        auto* stmt = m_arena.emplace<jump::continue_>(m_tokens.next());
        want_semicolon();
        return stmt;
      }
    case token_t::break_:
      {
        auto* stmt = m_arena.emplace<jump::break_>(m_tokens.next());
        want_semicolon();
        return stmt;
      }
    case token_t::return_:
      {
        auto return_       = m_tokens.next();
        auto* return_value = parse_expr();
        want_semicolon();
        return m_arena.emplace<jump::return_>(return_, return_value);
      }
    case token_t::label:
      {
        auto label_ = m_tokens.next();
        return m_arena.emplace<decl::label>(label_);
      }
    case token_t::goto_:
      {
        // Consume goto token
        m_tokens.advance();

        auto label = m_tokens.next();
        DEBUG_ASSERT(!label.value.empty());
        want_semicolon();
        return m_arena.emplace<jump::goto_>(label);
      }
    case token_t::o_curly:
      {
        auto* scope                    = &parse_compound();
        need_semicolon_after_statement = false;
        return scope;
      }
  }

  throw no_token_matched_exception(next, m_compound.top());
}

ast::decl::global_declaration& parser::parse_declaration() {
  if (!m_tokens.peek().type.is_specifier()) {
    throw std::exception();
  }
  auto mods         = parse_specifiers();
  const auto* ident = parse_identifier();
  if (m_tokens.peek().type == token_t::o_paren) {
    // Function
    auto& funcdecl                 = parse_function(std::move(mods), *ident);
    need_semicolon_after_statement = false;
    return funcdecl;
  }

  auto& vardecl = parse_variable(std::move(mods), ident);
  want_semicolon();
  return vardecl;
}
// FIXME
ast::expr::expression* parser::parse_lhs_expr() {
  const auto token = m_tokens.peek();

  if (token.type.is_literal()) {
    m_tokens.advance();
    return m_arena.emplace<expr::literal>(token);
  }

  if (token.type.is(token_t::ident)) {
    m_tokens.advance();
    return m_arena.emplace<expr::identifier>(token);
  }

  if (token.type.is(token_t::o_paren)) {
    m_tokens.advance();
    auto* expr = parse_expr(0);
    if (m_tokens.peek().type == token_t::c_paren) {
      m_tokens.advance();
    } else {
      throw std::runtime_error("Expected ')' after expression");
    }
    return expr;
  }

  if (token.type.is_operator()) {
    m_tokens.advance();
    auto* operand = parse_lhs_expr();
    term::operator_ op(token);
    return m_arena.emplace<expr::unary_operator>(*operand, std::move(op));
  }

  throw unexpected_token(token);
}

expr::expression* parser::parse_expr(uint8_t min_prec) {
  expr::expression* lhs = parse_lhs_expr();

  while (true) {
    const token& op = m_tokens.peek();

    if (!op.type.is_operator()) {
      break;
    }

    term::operator_ curr_op{op};
    auto prec = curr_op.type.precedence;

    if (prec < min_prec) {
      break;
    }

    m_tokens.advance();

    int next_min_prec =
        curr_op.type.assoc == cmm::associativity_t::L2R ? prec + 1 : prec;

    expr::expression* rhs = parse_expr(next_min_prec);

    auto* next_lhs =
        m_arena.emplace<expr::binary_operator>(*lhs, *rhs, std::move(curr_op));
    lhs = next_lhs;
  }

  return lhs;
}

expr::expression* parser::parse_call(const term::identifier& ident) {
  want(token_t::o_paren);

  if (m_tokens.peek().type == token_t::c_paren) {
    m_tokens.advance();
    // Zero parameters
    return m_arena.emplace<expr::call>(ident);
  }

  expr::call::arguments args;

  while (true) {
    expr::expression* expr = parse_expr();
    args.push_back(expr);

    if (m_tokens.peek().type == cmm::token_t::c_paren) {
      m_tokens.advance();
      break;
    }
    want(cmm::token_t::comma);
  }

  // No more parameters
  // but dont capture semicolon just jet
  return m_arena.emplace<expr::call>(ident, std::move(args));
}

ast::decl::specifiers parser::parse_specifiers() {
  ast::decl::specifiers specs;
  while (m_tokens.peek().type.is_specifier()) {
    specs.push_back(*m_arena.emplace<term::specifier>(m_tokens.next()));
  }
  return specs;
}

template <bool Optional>
term::identifier* parser::parse_identifier() {
  if (const auto& next = m_tokens.peek(); next.type == token_t::ident) {
    DEBUG_ASSERT(!next.value.empty(),
                 std::format("Identifier must have a value. token: {}", next));
    m_tokens.advance();
    return m_arena.emplace<term::identifier>(next);
  }
  if constexpr (!Optional) {
    throw unexpected_token(m_tokens.peek());
  }

  return nullptr;
}

decl::variable& parser::parse_variable(ast::decl::specifiers&& mods,
                                       const ast::term::identifier* id) {
  expr::expression* init = nullptr;
  if (m_tokens.peek().type == cmm::token_t::assign) {
    m_tokens.advance();
    init = parse_expr();
  }

  return *m_arena.emplace<decl::variable>(std::move(mods), id, init);
}

decl::function& parser::parse_function(decl::specifiers&& mods,
                                       const term::identifier& id) {
  want(cmm::token_t::o_paren);

  ast::decl::function::parameters_t siblings;

  if (m_tokens.peek().type != token_t::c_paren) {
    while (true) {
      decl::specifiers specs    = parse_specifiers();
      const token* next_        = &m_tokens.next();
      ast::term::identifier* id = nullptr;
      if (next_->type == token_t::ident) {
        id    = m_arena.emplace<ast::term::identifier>(*next_);
        next_ = &m_tokens.next();
      }
      expr::expression* e = nullptr;

      if (next_->type == token_t::eq) {
        e = parse_expr();
      }

      auto* var = m_arena.emplace<ast::decl::variable>(std::move(specs), id, e);
      siblings.push_back(*var);

      if (next_->type == cmm::token_t::c_paren) {
        break;
      }

      want(*next_, cmm::token_t::comma);
    }
  } else {
    // Closing paren
    m_tokens.advance();
  }

  // No more var decls
  // Block parsing
  compound* compound_ = nullptr;
  if (m_tokens.peek().type == token_t::o_curly) {
    compound_ = m_arena.emplace<compound>(parse_compound());
  }

  return *m_arena.emplace<decl::function>(
      std::move(mods), id, std::move(siblings), compound_);
}

expr::expression* parser::parser::parse_condition() {
  want(cmm::token_t::o_paren);
  expr::expression* condition{parse_expr()};
  want(cmm::token_t::c_paren);
  return condition;
}

statement* parser::parser::parse_while() {
  auto token       = m_tokens.next();
  auto* condition  = parse_condition();
  auto* while_stmt = parse_statement();
  return m_arena.emplace<iteration::while_>(token, *condition, while_stmt);
}

statement* parser::parser::parse_for() {
  auto token = m_tokens.next();
  want(token_t::o_paren);
  // If has start statement (var decl for now)
  decl::variable* start = nullptr;
  if (m_tokens.peek().type != token_t::semicolon) {
    auto mods      = parse_specifiers();
    const auto* id = parse_identifier();
    start          = &parse_variable(std::move(mods), id);
  }
  want_semicolon();

  expr::expression* condition = nullptr;
  if (m_tokens.peek().type != token_t::semicolon) {
    condition = parse_expr();
  }
  want(token_t::semicolon);

  expr::expression* step = nullptr;
  if (m_tokens.peek().type != token_t::c_paren) {
    step = parse_expr();
  }
  want(token_t::c_paren);

  auto* for_stmt = parse_statement();
  return m_arena.emplace<iteration::for_>(
      token, start, condition, step, for_stmt);
}

statement* parser::parser::parse_if() {
  auto token            = m_tokens.next();
  auto* condition       = parse_condition();
  statement* if_block   = parse_statement();

  statement* else_block = nullptr;

  if (m_tokens.has_next() && m_tokens.peek().type == cmm::token_t::else_) {
    m_tokens.advance();
    else_block = parse_statement();
  }

  return m_arena.emplace<selection::if_>(
      term::keyword(token), *condition, if_block, else_block);
}

void parser::parser::store_statement(ast::statement* stmt) {
  DEBUG_ASSERT(!m_compound.empty(), "Compound stack should not be empty");
  stmt->set_parent(&m_compound.top());
  m_compound.top().push_back(*stmt);
}

void parser::parser::want(const token& token,
                          const cmm::token_t& type,
                          bool needed_value) {
  if (token.type != type) {
    throw parser_exception(
        std::format("Wrong type. Wanted {}\n{}", type, token));
  }

  if (needed_value && !token.value.empty()) {
    throw parser_exception(std::format("Does not have value: {}", token));
  }
}

void parser::parser::want(const cmm::token_t& type, bool needed_value) {
  want(m_tokens.next(), type, needed_value);
}

void parser::parser::want_semicolon() {
  want(token_t::semicolon);

  while (m_tokens.has_next() && m_tokens.peek().type == token_t::semicolon) {
    m_tokens.advance();
  }
};

} // namespace cmm::parser
