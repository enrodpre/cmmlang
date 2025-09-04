#pragma once

#include "ast.hpp"
#include "common.hpp"
#include "token.hpp"
#include "types.hpp"
#include <cstdint>
#include <type_traits>
#include <utility>

namespace cmm::ast {

struct statement;

namespace expr {

struct semantic_data {
  bool loaded = false;
  std::optional<value_category_t> value_category;
  bool is_constant_evaluable{};
  types::type_id original_type;
  std::optional<types::type_id> casted_type;
};

struct expression : derived_visitable<expression, statement> {
  template <typename... Args>
  expression(Args&&... args)
      : derived_visitable(std::forward<Args>(args)...) {}
  ~expression() override = default;
  using derived_visitable::derived_visitable;

  semantic_data* semantics() const;
  virtual types::type_id type() const {
    const auto* sem = semantics();
    return sem->casted_type.has_value() ? sem->casted_type.value() : sem->original_type;
  }

private:
  mutable expr::semantic_data m_semantics;
};

static_assert(std::is_base_of_v<statement, expression>);

struct identifier : derived_visitable<identifier, expression> {
  identifier(ast::identifier&& id);
  std::string_view value() const { return m_term.value(); }
  operator ast::identifier() const { return m_term; }

  std::vector<node*> children() override { return {&m_term}; }

private:
  ast::identifier m_term;
};

static_assert(std::is_assignable_v<statement*&, expression*>);
DERIVE_OK(statement, expression);
DERIVE_OK(expression, identifier);

enum class literal_t : uint8_t { CHAR, STRING, SINT, UINT, FALSE, TRUE, FLOAT };
struct literal : derived_visitable<literal, expression> {
  literal_t category;
  literal(const token&, literal_t);

  std::string_view value() const { return m_term.value(); }
  operator ast::literal() const { return m_term; }

  std::vector<node*> children() override { return {&m_term}; }

protected:
  ast::literal m_term;
};

struct call : derived_visitable<call, expression> {
  ast::identifier ident;
  siblings<expr::expression*> args;

  call(decltype(ident)&& ident_, decltype(args) args);
  [[nodiscard]] std::vector<types::type_id> types() const {
    return args | std::views::transform([](const expr::expression* expr) -> types::type_id {
             return expr->type();
           }) |
           std::ranges::to<std::vector>();
  }

  std::vector<node*> children() override { return {&ident, &args}; }
};

struct unary_operator : derived_visitable<unary_operator, expression> {
  expression& expr;
  ast::operator_ operator_;
  unary_operator(expression& expression, ast::operator_&& op);
  std ::vector<node*> children() override { return std ::vector<node*>{expr, &operator_}; }
};

struct binary_operator : derived_visitable<binary_operator, expression> {
  expression& left;
  expression& right;
  ast::operator_ operator_;
  binary_operator(expression& left, ast::operator_&& op, expression& right);
  std ::vector<node*> children() override { return std ::vector<node*>{left, &operator_, right}; }
};

using conversion_function = std::function<expression&(expression&)>;

struct type_conversion : derived_visitable<type_conversion, expression> {
  expression& expr;
  const conversor func;

  type_conversion(expression& exp, conversor fn)
      : expr(exp),
        func(std::move(fn)) {}
};

struct implicit_type_conversion : derived_visitable<implicit_type_conversion, type_conversion> {
  using derived_visitable<implicit_type_conversion, type_conversion>::derived_visitable;
  children_impl(&expr);
};
}; // namespace expr
} // namespace cmm::ast
