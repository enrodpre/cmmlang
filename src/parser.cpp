#include "parser.hpp"

#include <algorithm>
#include <magic_enum/magic_enum.hpp>
#include <optional>
#include <ranges>
#include <stdexcept>
#include <type_traits>
#include <utility>

#include "ast.hpp"
#include "common.hpp"
#include "expr.h"
#include "lang.hpp"
#include "token.hpp"
#include "types.hpp"

namespace cmm::parser {

using namespace ast;
using namespace decl;

parser::parser(tokens tokens)
    : m_tokens(std::move(tokens)) {}

ast::translation_unit parser::parser::parse() { return parse_program(); }

ast::translation_unit parser::parser::parse_program() {
  siblings<declaration*> res;
  while (m_tokens.has_next()) {
    auto* decl = parse_declaration();
    res.push_back(decl);
  }
  return {res};
}

template <typename T>
  requires(std::is_same_v<ast::decl::function::definition, T> ||
           std::is_same_v<ast::decl::block, T>)
T* parser::parse_block() {
  want(token_t::o_curly);
  // TODO: Allocating this is leaking the statement* pointer
  ast::siblings<ast::statement*> b;

  while (m_tokens.has_next()) {
    if (m_tokens.peek().type == token_t::c_curly) {
      m_tokens.advance();
      return create_node<T>(std::move(b));
    }
    if (next_is(token_t::semicolon)) {
      m_tokens.advance();
      continue;
    }

    auto* stmt = parse_statement();
    if (stmt != nullptr) {
      b.push_back(stmt);
    }
  }

  auto tok = m_tokens.peek();
  THROW(UNEXPECTED_TOKEN, tok);
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
  if (peek_data().is_specifier()) {
    // It's a decl
    return parse_declaration();
  }

  if (peek_data().is_operator()) {
    auto b = consume_semicolon();
    return &parse_expr();
  }

  switch (m_tokens.peek().type) { // NOLINT

    // If identifier, can be a function call or a var assignment
    case token_t::ident:
      {
        auto b = consume_semicolon();
        return &parse_expr();
        break;
      }
    case token_t::if_:
      {
        return parse_if();
      }
    case token_t::while_:
      {

        return parse_while();
      }
    case token_t::for_:
      {
        return parse_for();
      }
    case token_t::continue_:
      {
        auto consumer = consume_semicolon();
        return create_node<jump::continue_>(m_tokens.next());
      }
    case token_t::break_:
      {
        auto consumer = consume_semicolon();
        return create_node<jump::break_>(m_tokens.next());
      }
    case token_t::return_:
      {
        auto return_       = m_tokens.next();
        auto* return_value = &parse_expr();
        auto consumer      = consume_semicolon();
        return create_node<jump::return_>(std::move(return_), return_value);
      }
    case token_t::label:
      return create_node<decl::label>(m_tokens.next());
    case token_t::goto_:
      {
        // Consume goto token
        m_tokens.advance();

        auto label    = m_tokens.next();
        auto consumer = consume_semicolon();
        return create_node<jump::goto_>(label);
      }
    case token_t::o_curly:
      {
        return parse_block();
      }
  }

  REGISTER_ERROR("No token matched {}", next);
  THROW(UNEXPECTED_TOKEN, next);
}

ast::declaration* parser::parse_declaration() {
  if (!peek_data().is_specifier()) {
    throw std::exception();
  }
  auto mods  = parse_specifiers();
  auto ident = parse_identifier();
  if (m_tokens.peek().type == token_t::o_paren) {
    // Function
    auto* funcdecl = parse_function(std::move(mods), std::move(ident));
    return funcdecl;
  }

  auto* vardecl = parse_variable(std::move(mods), std::move(ident));
  want_semicolon();
  return vardecl;
}

#define CREATE_LITERAL(TOKEN, TYPE) create_node<expr::TYPE>(token);

ast::expr::expression& parser::parse_lhs_expr() {
  const auto& token = m_tokens.peek();

  if (peek_data().is_literal()) {
    auto lit = m_tokens.next();
    switch (lit.type) {
      case token_t::false_lit:
        return *create_node<expr::literal>(lit, expr::literal_t::FALSE);
      case token_t::true_lit:
        return *create_node<expr::literal>(lit, expr::literal_t::TRUE);
      case token_t::int_lit:
        return *create_node<expr::literal>(lit, expr::literal_t::SINT);
      case token_t::float_lit:
        return *create_node<expr::literal>(lit, expr::literal_t::FLOAT);
      case token_t::string_lit:
        return *create_node<expr::literal>(lit, expr::literal_t::STRING);
      case token_t::char_lit:
        return *create_node<expr::literal>(lit, expr::literal_t::CHAR);
      default:
        break;
    }
  }

  if (peek_data().is(token_t::ident)) {
    auto ident = parse_identifier();
    if (m_tokens.next_is(token_t::o_paren)) {
      return parse_call(std::move(ident));
    }

    return *create_node<expr::identifier>(std::move(ident));
  }

  if (peek_data().is(token_t::o_paren)) {
    m_tokens.advance();
    auto& expr = parse_expr();

    if (!m_tokens.next_is(token_t::c_paren)) {
      throw std::runtime_error("Expected ')' after expression");
    }

    m_tokens.advance();
    return expr;
  }

  if (peek_data().is_unary_operator()) {
    auto next                 = m_tokens.next();
    operator_ t               = next.type == token_t::dec   ? operator_(next, operator_t::pre_dec)
                                : next.type == token_t::inc ? operator_(next, operator_t::pre_inc)
                                                            : operator_(token);
    expr::expression& operand = parse_lhs_expr();
    return *create_node<expr::unary_operator>(operand, std::move(t));
  }

  THROW(UNEXPECTED_TOKEN, token);
}

expr::expression& parser::parse_expr(uint8_t min_prec) {
  expr::expression& lhs = parse_lhs_expr();

  while (m_tokens.has_next()) {
    const token& op_token = m_tokens.peek();

    if (!peek_data().is_binary_operator()) {
      break;
    }
    if (op_token.type == token_t::inc || op_token.type == token_t::dec) {
      m_tokens.advance();
      operator_ t(op_token,
                  op_token.type == token_t::inc ? operator_t::post_inc : operator_t::post_dec);
      lhs = *create_node<expr::unary_operator>(lhs, std::move(t));
      continue;
    }

    operator_ curr_op{op_token};
    operator_data data(curr_op.value());
    auto prec = data.precedence;

    if (prec >= min_prec) {
      break;
    }

    m_tokens.advance();

    uint8_t next_min_precedence = prec;
    if (data.assoc == cmm::associativity_t::L2R) {
      next_min_precedence++;
    }

    expr::expression& rhs = parse_expr(next_min_precedence);
    lhs                   = *create_node<expr::binary_operator>(lhs, std::move(curr_op), rhs);
  }

  return lhs;
}

expr::expression& parser::parse_call(identifier&& ident) {
  auto p = [this]() -> expr::expression* { return &this->parse_expr(); };
  siblings<expr::expression*> args =
      parse_varargs(p, token_t::o_paren, token_t::comma, token_t::c_paren);

  // No more parameters
  // but dont capture semicolon just jet
  return *create_node<expr::call>(std::move(ident), args);
}

identifier parser::parse_identifier() {
  if (m_tokens.next_is(token_t::ident)) {
    return {m_tokens.next()};
  }

  throw_error<compilation_error_t::UNEXPECTED_TOKEN>(m_tokens.peek());
}

decl::variable* parser::parse_variable(ast::decl::specifiers&& mods, ast::identifier&& id) {
  decl::rank* rank       = parse_rank();
  expr::expression* init = nullptr;
  if (m_tokens.peek().type == cmm::token_t::assign) {
    m_tokens.advance();
    init = &parse_expr();
  }

  return create_node<decl::variable>(std::move(mods), rank, id, init);
}

namespace {
linkage_spec parse_linkage(const std::vector<token>& ts) {
  auto res =
      std::ranges::find_if(ts, [](const auto& spec) { return spec.type == token_t::static_; });
  if (res != ts.cend()) {
    return {*res, linkage_t::internal};
  }
  return {};
}

ast::storage_spec parse_storage(const std::vector<token>& ts) {
  auto storages =
      ts | std::views::filter([](const auto& spec) { return token_data(spec.type).is_storage(); }) |
      std::ranges::to<std::vector>();
  if (storages.size() == 0) {
    return {};
  }
  if (storages.size() == 1) {
    return {storages[0], storage_t::static_};
  }

  throw_error<compilation_error_t::INCOMPATIBLE_TOKEN>(storages[1]);
}

constexpr types::core_t parse_enum_type(const token_data& token_type, bool unsigned_) {
  if (token_type == token_t::int_t) {
    return unsigned_ ? types::core_t::uint_t : types::core_t::sint_t;
  } // namespace cmm::ast
  return token_type.cast<types::core_t>();
}

types::type_id parse_type(const std::vector<token>& ts) {
  types::cv_qualification_t const_;
  types::cv_qualification_t volatile_;
  bool unsigned_ = false;
  std::optional<token_t> type_;
  for (const auto& t : ts) {
    if (t.type == token_t::const_) {
      const_ = types::cv_qualification_t::CONST;
    } else if (t.type == token_t::volatile_) {
      volatile_ = types::cv_qualification_t::VOLATILE;
    } else if (token_data(t.type).is_type()) {
      type_.emplace(t.type);
    } else if (t.type == token_t::unsigned_) {
      unsigned_ = true;
    }
  }

  if (!type_.has_value()) {
    auto r = ts | std::views::transform([](const auto& spec) -> std::optional<cmm::location> {
               return spec.location();
             }) |
             std::ranges::to<std::vector>();

    THROW(REQUIRED_TYPE, std::ranges::fold_left_first(r, std::plus{}).value());
  }
  return types::global().make(parse_enum_type(type_.value(), unsigned_), {}, const_ | volatile_);
}

template <ast_node Parent, ast_node Node, ast_node... Nodes>
void set_parent_node(Parent* parent, Node&& node, Nodes&&... args) {
  if constexpr (requires { node.operator->(); }) {
    node->set_parent(parent);
  } else if constexpr (std::is_pointer_v<Node>) {
    node->set_parent(parent);
  } else {
    node.set_parent(parent);
  }
  set_parent_node(parent, std::forward<Nodes>(args)...);
}
} // namespace

ast::decl::rank* parser::parse_rank() {
  decl::rank* res = nullptr;
  if (m_tokens.next_is(token_t::o_bracket)) {
    const token& left   = m_tokens.next();
    expr::expression* r = nullptr;
    if (!m_tokens.next_is(token_t::c_bracket)) {
      r = &parse_expr();
    }
    if (r == nullptr && !m_tokens.next_is(token_t::c_bracket)) {
      throw_error<compilation_error_t::UNEXPECTED_TOKEN>(m_tokens.peek());
    }
    const token& right = want(token_t::c_bracket);
    res                = create_node<decl::rank>(left, r, right);
  }
  return res;
}

ast::decl::specifiers parser::parse_specifiers() {
  std::vector<token> specs;
  while (peek_data().is_specifier()) {
    specs.push_back(m_tokens.next());
  }

  return {parse_type(specs), parse_linkage(specs), parse_storage(specs)};
}

template <typename Func, typename T>
siblings<T> parser::parse_varargs(Func&& inner,
                                  const token_t& opener,
                                  const token_t& delim,
                                  const token_t& closer) {
  siblings<T> res;
  want(opener);
  if (m_tokens.next_is(closer)) {
    m_tokens.advance();
    return res;
  }

  while (true) {
    res.push_back(inner());

    if (m_tokens.next_is(closer)) {
      break;
    }

    want(delim);
  }
  m_tokens.advance();

  return res;
}

decl::function* parser::parse_function(decl::specifiers&& mods, identifier&& ident) {
  auto p = [this]() -> variable* {
    decl::specifiers specs = parse_specifiers();
    auto id                = parse_identifier();
    auto* rank             = parse_rank();
    expr::expression* e    = nullptr;
    if (m_tokens.next_is(token_t::eq)) {
      e = &parse_expr();
    }

    return create_node<variable>(std::move(specs), rank, id, e);
  };

  auto params = parse_varargs(p, token_t::o_paren, token_t::comma, token_t::c_paren);

  // No more var decls
  // Block parsing
  ast::decl::function::definition* compound_ = nullptr;
  if (m_tokens.peek().type == token_t::o_curly) {
    compound_ = parse_block<function::definition>();
  }

  auto* func =
      create_node<decl::function>(std::move(mods), std::move(ident), std::move(params), compound_);
  return func;
}

expr::expression& parser::parser::parse_condition() {
  want(cmm::token_t::o_paren);
  expr::expression& condition{parse_expr()};
  want(cmm::token_t::c_paren);
  return condition;
}

statement* parser::parser::parse_while() {
  auto token       = m_tokens.next();
  auto& condition  = parse_condition();
  auto* while_stmt = parse_block();
  return create_node<iteration::while_>(token, condition, while_stmt);
}

statement* parser::parser::parse_for() {
  auto token = m_tokens.next();
  want(token_t::o_paren);
  // If has start statement (var decl for now)
  decl::variable* start = nullptr;
  if (m_tokens.peek().type != token_t::semicolon) {
    auto specs = parse_specifiers();
    auto ident = parse_identifier();
    start      = parse_variable(std::move(specs), std::move(ident));
  }
  want_semicolon();

  expr::expression* condition = nullptr;
  if (m_tokens.peek().type != token_t::semicolon) {
    condition = &parse_expr();
  }
  want(token_t::semicolon);

  expr::expression* step = nullptr;
  if (m_tokens.peek().type != token_t::c_paren) {
    step = &parse_expr();
  }
  want(token_t::c_paren);

  auto* for_stmt = parse_block();
  return create_node<iteration::for_>(token, start, condition, step, for_stmt);
}

statement* parser::parser::parse_if() {
  const auto& token           = m_tokens.next();
  expr::expression& condition = parse_condition();
  block* if_block             = parse_block();

  block* else_block           = nullptr;

  if (m_tokens.has_next() && m_tokens.peek().type == cmm::token_t::else_) {
    m_tokens.advance();
    else_block = parse_block();
  }

  return create_node<selection::if_>(token, condition, if_block, else_block);
}

void parser::parser::want(const token& token, const cmm::token_t& type, bool needed_value) {
  if (token.type != type) {
    THROW(UNEXPECTED_TOKEN, token);
  }

  if (needed_value && !token.value.empty()) {
    THROW(UNEXPECTED_TOKEN, token);
    ;
  }
}

[[nodiscard]] token_data parser::peek_data() const { return {m_tokens.peek().type}; }

[[nodiscard]] bool parser::next_is(token_t t) const { return m_tokens.next_is(t); }

token parser::parser::want(const cmm::token_t& type, bool needed_value) {
  const auto& tok = m_tokens.next();
  want(tok, type, needed_value);
  return tok;
}

void parser::parser::want_semicolon() {
  want(token_t::semicolon);

  while (m_tokens.has_next() && m_tokens.peek().type == token_t::semicolon) {
    m_tokens.advance();
  }
};

template <typename T, typename... Args>
  requires(std::is_constructible_v<T, Args...>)
T* parser::create_node(Args&&... args) {
  T* obj = m_arena.emplace<T>(std::forward<Args>(args)...);
  obj->initialize();
  return obj;
}

} // namespace cmm::parser
