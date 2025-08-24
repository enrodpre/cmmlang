
#include "expr.h"

#include "ast.hpp"
#include "semantic.hpp"
#include <utility>

namespace cmm::ast {

expr::semantic_data* expr::expression::semantics() const {
  if (!m_semantics.loaded) {
    m_semantics.loaded = true;
    semantics::load_node_semantics(*this);
  }
  return &m_semantics;
}

expr::identifier::identifier(ast::identifier&& id)
    : m_term(std::move(id)) {}

expr::literal::literal(cmm::location l, std::string s, literal_t t)
    : category(t),
      m_term(std::move(l), std::move(s)) {}

expr::literal::literal(const token& t, literal_t l)
    : literal(t.location(), t.value, l) {}

expr::call::call(decltype(ident)&& id, decltype(args)&& a = {})
    : ident(std::move(id)),
      args(std::move(a)) {}

expr::unary_operator::unary_operator(expression* expression, ast::operator_&& op)
    : expr(*expression),
      operator_(std::move(op)) {}

expr::binary_operator::binary_operator(expression* l, ast::operator_&& op, expression* r)
    : left(*l),
      right(*r),
      operator_(std::move(op)) {}
} // namespace cmm::ast
