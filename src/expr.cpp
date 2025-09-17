#include "expr.h"

#include "ast.hpp"
#include "ast_visitor.hpp"
#include "common.hpp"
#include "ir.hpp"
#include "lang.hpp"
#include "types.hpp"
#include <utility>

namespace cmm::ast::expr {

namespace {
template <typename T>
using ret_t = retriever_visitor<VisitorDirection::ChildToParent, T*>;
template <typename T>
ret_t<T> get_retriever() {
  return retriever_visitor<VisitorDirection::ChildToParent, T*>{
      [](const node* t_node) { return dynamic_cast<const T*>(t_node) != nullptr; }};
}
} // namespace

types::type_id expr::expression::type() const {
  return current_conversor.has_value() ? current_conversor->convert_type(type_impl()) : type_impl();
}
value_category_t expr::expression::value_category() const {
  return current_conversor.has_value()
             ? current_conversor->convert_value_category(value_category_impl())
             : value_category_impl();
}

expr::identifier::identifier(const token& t_token)
    : m_term(t_token) {}

type_id expr::identifier::type_impl() const {
  return get_root()->get_variable(*this).first->specs.type.value();
}

expr::literal::literal(const token& t, literal_t l)
    : category(l),
      m_term(t.location(), t.value) {}

type_id expr::literal::type_impl() const {
  switch (category) {
    case ast::expr::literal_t::CHAR:
      return CHAR_T;
      break;
    case ast::expr::literal_t::STRING:
      return types::make(types::core_t::char_t, {types::layer_t::array_t});
      break;
    case ast::expr::literal_t::SINT:
      return SINT_T;
      break;
    case ast::expr::literal_t::UINT:
      return UINT_T;
      break;
    case ast::expr::literal_t::FALSE:
    case ast::expr::literal_t::TRUE:
      return BOOL_T;
      break;
    case ast::expr::literal_t::FLOAT:
      return FLOAT_T;
      break;
      DEFAULT_CASE;
  }
}
expr::call::call(decltype(ident)&& id, decltype(args) a = {})
    : ident(std::move(id)),
      args(std::move(a)) {}

type_id expr::call::type_impl() const {
  const auto* func = get_root()->get_callable(ident, args);
  return func->return_type();
}

value_category_t expr::call::value_category_impl() const { return get_value_category(type_impl()); }

expr::unary_operator::unary_operator(expression& expression, ast::operator_&& op)
    : expr(expression),
      operator_(std::move(op)) {}

type_id expr::unary_operator::type_impl() const {
  const auto* func = get_root()->get_callable(operator_, {&expr.get()});
  return func->return_type();
}
value_category_t expr::unary_operator::value_category_impl() const {
  return get_value_category(type_impl());
}
bool expr::binary_operator::is_constant_evaluable() const {
  return left->is_constant_evaluable() && right->is_constant_evaluable() &&
         get_root()
             ->get_callable(operator_, expr::arguments{&left.get(), &right.get()})
             ->is_user_defined();
}
expr::binary_operator::binary_operator(expression& l, ast::operator_&& op, expression& r)
    : left(l),
      operator_(std::move(op)),
      right(r) {}

type_id expr::binary_operator::type_impl() const {
  const auto* func = get_root()->get_callable(operator_, {&left.get(), &right.get()});
  return func->return_type();
}

value_category_t expr::binary_operator::value_category_impl() const {
  return get_value_category(type_impl());
}
} // namespace cmm::ast::expr
