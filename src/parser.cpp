#include "parser.hpp"
#include "ast.hpp"
#include "common.hpp"

#include "lang.hpp"
#include "token.hpp"
#include "types.hpp"
#include <magic_enum/magic_enum_format.hpp>
#include <utility>

namespace cmm::parser {

using namespace ast;
using namespace scope;
// using namespace messaging;

parser::parser(tokens tokens)
    : m_tokens(std::move(tokens)) {}

program parser::parser::parse() { return parse_program(); }

ast::program parser::parser::parse_program() {
  while (m_tokens.has_next()) {
    auto* decl = parse_declaration();
    m_global.push_back(decl);
  }
  return {std::move(m_global)};
}

block* parser::parse_compound() {
  want(token_t::o_curly);
  std::vector<ast::statement*> comp;

  while (m_tokens.has_next()) {
    if (m_tokens.peek().type == token_t::c_curly) {
      m_tokens.advance();
      need_semicolon_after_statement = false;
      return create_node<block>(std::move(comp));
    }

    auto* stmt = parse_statement();
    // stmt->set_parent(&m_compound.top());
    comp.push_back(stmt);
  }

  auto tok = m_tokens.peek();
  throw_error<_error_t::UNEXPECTED_TOKEN>(tok);
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
    return parse_declaration();
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
        auto* stmt = create_node<jump::continue_>(m_tokens.next());
        want_semicolon();
        return stmt;
      }
    case token_t::break_:
      {
        auto* stmt = create_node<jump::break_>(m_tokens.next());
        want_semicolon();
        return stmt;
      }
    case token_t::return_:
      {
        auto return_       = m_tokens.next();
        auto* return_value = parse_expr();
        want_semicolon();
        return create_node<jump::return_>(return_, return_value);
      }
    case token_t::label:
      {
        auto label_ = m_tokens.next();
        return create_node<decl::label>(label_);
      }
    case token_t::goto_:
      {
        // Consume goto token
        m_tokens.advance();

        auto label = m_tokens.next();
        DEBUG_ASSERT(!label.value.empty());
        want_semicolon();
        return create_node<jump::goto_>(label);
      }
    case token_t::o_curly:
      {
        auto* scope                    = parse_compound();
        need_semicolon_after_statement = false;
        return scope;
      }
  }

  REGISTER_WARN("No token matched");
  NOT_IMPLEMENTED;
  return nullptr;
}

ast::global_statement* parser::parse_declaration() {
  if (!m_tokens.peek().type.is_specifier()) {
    throw std::exception();
  }
  auto mods  = parse_specifiers();
  auto ident = parse_identifier();
  if (m_tokens.peek().type == token_t::o_paren) {
    // Function
    auto* funcdecl                 = parse_function(std::move(mods), std::move(ident));
    need_semicolon_after_statement = false;
    return funcdecl;
  }

  auto* vardecl = parse_variable(std::move(mods), std::move(ident));
  want_semicolon();
  return vardecl;
}

#define CREATE_LITERAL(TOKEN, TYPE) create_node<expr::TYPE>(token);
// FIXME
ast::expr::expression* parser::parse_lhs_expr() {
  const auto& token = m_tokens.peek();

  if (token.type.is_literal()) {
    m_tokens.advance();
    switch (token.type.inner()) {
      case _token_t::false_lit:
        return create_node<expr::false_lit>(token);
      case _token_t::true_lit:
        return create_node<expr::true_lit>(token);
      case _token_t::int_lit:
        return create_node<expr::int_lit>(token);
      case _token_t::float_lit:
        return create_node<expr::float_lit>(token);
      case _token_t::string_lit:
        return create_node<expr::string_lit>(token);
      case _token_t::char_lit:
        return create_node<expr::char_lit>(token);
      default:
        NOT_IMPLEMENTED;
        break;
    }
  }

  expr::expression* expr = nullptr;
  if (token.type.is(token_t::ident)) {
    auto ident = parse_identifier();
    if (m_tokens.next_is(token_t::o_paren)) {
      return parse_call(std::move(ident));
    }

    return create_node<expr::identifier>(std::move(ident));
  }

  if (token.type.is(token_t::o_paren)) {
    m_tokens.advance();
    auto* expr = parse_expr();
    if (m_tokens.peek().type == token_t::c_paren) {
      m_tokens.advance();
    } else {
      throw std::runtime_error("Expected ')' after expression");
    }
    return expr;
  }

  if (token.type.is_operator()) {
    m_tokens.advance();
    operator_t op;
    if (token.type.is(token_t::dec)) {
      op = operator_t::pre_dec;
    } else if (token.type.is(token_t::inc)) {
      op = operator_t::pre_inc;
    } else if (token.type.is_castable<operator_t>()) {
      op = token.type.cast<operator_t>();
    } else {
      NOT_IMPLEMENTED;
    }
    term::operator_ term(token, op);
    auto* operand = parse_lhs_expr();
    return create_node<expr::unary_operator>(operand, std::move(term));
  }

  throw_error<error_t::UNEXPECTED_TOKEN>(token);
}

expr::expression* parser::parse_expr(uint8_t min_prec) {
  expr::expression* lhs = parse_lhs_expr();

  while (true) {
    const token& op = m_tokens.peek();

    if (!op.type.is_operator()) {
      break;
    }
    if (op.type == token_t::inc || op.type == token_t::dec) {
      m_tokens.advance();
      operator_t op_t;
      if (op.type == token_t::inc) {
        op_t = operator_t::post_inc;
      } else if (op.type == token_t::dec) {
        op_t = operator_t::post_dec;
      } else {
        NOT_IMPLEMENTED;
      }
      term::operator_ t(op, op_t);
      lhs = create_node<expr::unary_operator>(lhs, std::move(t));
      continue;
    }

    term::operator_ curr_op{op};
    auto prec = curr_op.type.precedence;

    if (prec >= min_prec) {
      break;
    }

    m_tokens.advance();

    if (curr_op.type.assoc == cmm::associativity_t::L2R) {
      prec++;
    }

    expr::expression* rhs = parse_expr(prec);
    lhs                   = create_node<expr::binary_operator>(lhs, rhs, std::move(curr_op));
  }

  return lhs;
}

expr::expression* parser::parse_call(term::identifier&& ident) {
  auto p = [this]() { return this->parse_expr(); };
  auto args =
      parse_varargs<expr::expression*>(p, token_t::o_paren, token_t::comma, token_t::c_paren);

  // No more parameters
  // but dont capture semicolon just jet
  return create_node<expr::call>(std::move(ident), std::move(args));
}

term::identifier parser::parse_identifier() {
  if (m_tokens.next_is(token_t::ident)) {
    auto&& next = m_tokens.next();
    DEBUG_ASSERT(!next.value.empty(), std::format("Identifier must have a value. token: {}", next));
    return {next};
  }

  throw_error<error_t::UNEXPECTED_TOKEN>(m_tokens.peek());
}

decl::variable* parser::parse_variable(ast::decl::specifiers&& mods, ast::term::identifier&& id) {
  expr::expression* init = nullptr;
  if (m_tokens.peek().type == cmm::token_t::assign) {
    m_tokens.advance();
    init = parse_expr();
  }

  return create_node<decl::variable>(std::move(mods), std::move(id), init);
}

namespace {
  term::linkage parse_linkage(const std::vector<token>& ts) {
    auto res =
        std::ranges::find_if(ts, [](const auto& spec) { return spec.type == token_t::static_; });
    if (res != ts.cend()) {
      return linkage_t::internal;
    }
    return linkage_t::normal;
  }

  term::storage parse_storage(const std::vector<token>& ts) {
    auto storages = ts |
                    std::views::filter([](const auto& spec) { return spec.type.is_storage(); }) |
                    std::ranges::to<std::vector>();
    if (storages.size() == 0) {
      return storage_t::normal;
    }
    if (storages.size() == 1) {
      return {storage_t::static_};
    }

    throw_error<error_t::INCOMPATIBLE_TOKEN>(storages[1], storages[1], storages[0]);
  }
  constexpr type_category_t parse_enum_type(const token_t& token_type, bool unsigned_) {
    if (token_type == token_t::int_t) {
      return unsigned_ ? type_category_t::uint_t : type_category_t::sint_t;
    } // namespace cmm::ast
    return token_type.cast<type_category_t>();
  }
  cr_type parse_type(const std::vector<token>& ts) {
    bool const_    = false;
    bool volatile_ = false;
    bool unsigned_ = false;
    std::optional<token_t> type_;
    for (const auto& t : ts) {
      if (t.type == token_t::const_) {
        const_ = true;
      } else if (t.type == token_t::volatile_) {
        volatile_ = true;
      } else if (t.type.is_type()) {
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

      // throw_error<error_t::REQUIRED_TYPE>(t);
    }
    return type::create_fundamental(parse_enum_type(type_.value(), unsigned_), const_, volatile_);
  }

  void set_parent_node() {}
  template <is_node Parent, is_node Node, is_node... Nodes>
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

ast::decl::specifiers parser::parse_specifiers() {
  std::vector<token> specs;
  while (m_tokens.peek().type.is_specifier()) {
    specs.push_back(m_tokens.next());
  }

  ast::decl::specifiers&& res = {parse_type(specs), parse_linkage(specs), parse_storage(specs)};
  return std::move(res);
}

template <typename T, typename... Args>
T* parser::create_node(Args&&... args) {
  auto* obj = m_arena.emplace<T>(std::forward<Args>(args)...);
  // set_parent_node(obj, std::forward<Args>(args)...);
  return obj;
}

template <typename T, typename Func>
std::vector<T> parser::parse_varargs(Func&& inner,
                                     const token_t& opener,
                                     const token_t& delim,
                                     const token_t& closer) {
  std::vector<T> res;
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

decl::function* parser::parse_function(decl::specifiers&& mods, term::identifier&& id) {
  auto p = [this]() -> decl::function::parameter {
    decl::specifiers specs = parse_specifiers();

    std::optional<term::identifier> id;
    if (m_tokens.next_is(token_t::ident)) {
      id.emplace(m_tokens.next());
    }

    expr::expression* e = nullptr;
    if (m_tokens.next_is(token_t::eq)) {
      e = parse_expr();
    }

    return {specs, id.value_or({}), e};
  };

  auto params = parse_varargs<decl::function::parameter>(
      p, token_t::o_paren, token_t::comma, token_t::c_paren);

  // No more var decls
  // Block parsing
  function* compound_ = nullptr;
  if (m_tokens.peek().type == token_t::o_curly) {
    compound_ = parse_compound();
  }

  auto* func =
      create_node<decl::function>(std::move(mods), std::move(id), std::move(params), compound_);
  return func;
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
  return create_node<iteration::while_>(token, *condition, while_stmt);
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
    condition = parse_expr();
  }
  want(token_t::semicolon);

  expr::expression* step = nullptr;
  if (m_tokens.peek().type != token_t::c_paren) {
    step = parse_expr();
  }
  want(token_t::c_paren);

  auto* for_stmt = parse_statement();
  return create_node<iteration::for_>(token, start, condition, step, for_stmt);
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

  return create_node<selection::if_>(term::keyword(token), *condition, if_block, else_block);
}

void parser::parser::want(const token& token, const cmm::token_t& type, bool needed_value) {
  if (token.type != type) {
    throw parser_exception(std::format("Wrong type. Wanted {}\n{}", type, token));
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
