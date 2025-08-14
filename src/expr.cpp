
#include "expr.h"
#include "ast.hpp"
#include "visitor.hpp"

namespace cmm::ast {

expr::expression::expression()
    : semantics() {}

expr::identifier::identifier(ast::identifier&& id)
    : m_term(std::move(id)) {
  m_term.set_parent(this);
}

expr::literal::literal(cmm::location l, std::string s)
    : m_term(std::move(l), std::move(s)) {}
expr::literal::literal(const token& t)
    : literal(t.location(), t.value) {}

expr::string_literal::string_literal(const token& t)
    : visitable(t) {}
expr::false_literal::false_literal(const token& t)
    : visitable(t.location(), "0") {}
expr::true_literal::true_literal(const token& t)
    : visitable(t.location(), "1") {}
expr::sint_literal::sint_literal(const token& t)
    : visitable(t) {}
expr::uint_literal::uint_literal(const token& t)
    : visitable(t) {}
expr::float_literal::float_literal(const token& t)
    : visitable(t) {}
expr::char_literal::char_literal(const token& t)
    : visitable(t) {}

expr::call::call(decltype(ident)&& id, decltype(args)&& a = {})
    : ident(std::move(id)),
      args(std::move(a)) {
  ident.set_parent(this);
  args.set_parent(this);
}
cmm::location expr::call::location() const { return ident.location() + args.location(); }

expr::unary_operator::unary_operator(expression* expression, ast::operator_&& op)
    : expr(*expression),
      operator_(std::move(op)) {
  expr.set_parent(this);
  operator_.set_parent(this);
}
cmm::location expr::unary_operator::location() const {
  return expr.location() + operator_.location();
}
cmm::location expr::binary_operator::location() const {
  return left.location() + operator_.location() + right.location();
}

bool expr::expression::is_category(value_category_t cat) const {
  return semantics.value_category == cat;
}
void expr::expression::load_semantics(ptr_type t, value_category_t v) const {
  semantics.loaded         = true;
  semantics.original_type  = t;
  semantics.value_category = v;
}

expr::binary_operator::binary_operator(expression* left, expression* right, ast::operator_&& op)
    : left(*left),
      right(*right),
      operator_(std::move(op)) {
  left->set_parent(this);
  right->set_parent(this);
  operator_.set_parent(this);
}
} // namespace cmm::ast
