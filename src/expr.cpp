
#include "expr.h"
#include "ast.hpp"
#include "visitor.hpp"

namespace cmm::ast {

expr::expression::expression()
    : semantics() {}

expr::identifier::identifier(ast::terms::identifier&& id)
    : visitable(std::move(id)) {
  // terms::identifier::set_parent(this);
}
void expr::expression::load_semantics(ptr_type t, value_category_t v) const {
  semantics.loaded         = true;
  semantics.original_type  = t;
  semantics.value_category = v;
}
bool expr::expression::is_category(value_category_t cat) const {
  return semantics.value_category == cat;
}
cmm::location expr::identifier::location() const { return location(); }

expr::literal::literal(cmm::location l, std::string s)
    : terms::literal(std::move(l), std::move(s)) {
  // term.set_parent(this);
}
// expr::literal::literal(std::string str)
//     : terms::literal(str) {}

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

expr::call::call(decltype(ident)&& ident_, decltype(args)&& args)
    : ident(std::move(ident_)),
      args(std::move(args)) {
  // ident.set_parent(this);
  args.set_parent(this);
}
cmm::location expr::call::location() const { return ident.location() + args.location(); }

expr::unary_operator::unary_operator(expression* expression, terms::operator_&& op)
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
expr::expression::expression()
    : semantics() {}

bool expr::expression::is_category(value_category_t cat) const {
  return semantics.value_category == cat;
}
void expr::expression::load_semantics(ptr_type t, value_category_t v) const {
  semantics.loaded         = true;
  semantics.original_type  = t;
  semantics.value_category = v;
}

expr::binary_operator::binary_operator(expression* left, expression* right, terms::operator_&& op)
    : left(*left),
      right(*right),
      operator_(std::move(op)) {
  left->set_parent(this);
  right->set_parent(this);
  operator_.set_parent(this);
}
expr::binary_operator::binary_operator(expression* left, expression* right, terms::operator_&& op)
    : left(*left),
      right(*right),
      operator_(std::move(op)) {
  left->set_parent(this);
  right->set_parent(this);
  operator_.set_parent(this);
}
} // namespace cmm::ast
