#pragma once

#include "ast.hpp"
#include "common.hpp"
#include "token.hpp"
#include "types.hpp"
#include <cstdint>
#include <utility>

namespace cmm::ast {

struct statement;

namespace expr {

struct conversion;

struct expression : derived_visitable<expression, statement> {
  template <typename... Args>
  expression(Args&&... args)
      : derived_visitable(std::forward<Args>(args)...) {}
  ~expression() override = default;
  using derived_visitable::derived_visitable;

  types::type_id type() const;
  value_category_t value_category() const;
  virtual bool is_constant_evaluable() const = 0;

  std::optional<conversor> current_conversor;

protected:
  virtual types::type_id type_impl() const             = 0;
  virtual value_category_t value_category_impl() const = 0;
};

using expression_reference = reference<expression>;
using expressions          = std::vector<expression_reference>;

struct identifier : derived_visitable<identifier, expression> {
  identifier(const token&);
  std::string_view value() const { return m_term.value(); }
  operator ast::identifier() const { return m_term; }

  std::vector<node*> children() override { return {&m_term}; }
  types::type_id type_impl() const override;
  value_category_t value_category_impl() const override { return value_category_t::LVALUE; };
  bool is_constant_evaluable() const override { return types::is_const(type()); }

private:
  ast::identifier m_term;
};

enum class literal_t : uint8_t { CHAR, STRING, SINT, UINT, FALSE, TRUE, FLOAT };
struct literal : derived_visitable<literal, expression> {
  literal_t category;
  literal(const token&, literal_t);

  std::string_view value() const { return m_term.value(); }
  operator ast::literal() const { return m_term; }

  std::vector<node*> children() override { return {&m_term}; }
  bool is_constant_evaluable() const override { return true; };
  types::type_id type_impl() const override;
  value_category_t value_category_impl() const override { return value_category_t::PRVALUE; }

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
  bool is_constant_evaluable() const override { return false; };
  types::type_id type_impl() const override;
  value_category_t value_category_impl() const override;
};

struct unary_operator : derived_visitable<unary_operator, expression> {
  reference<expression> expr;
  ast::operator_ operator_;
  unary_operator(expression& expression, ast::operator_&& op);
  std ::vector<node*> children() override { return std ::vector<node*>{&expr, &operator_}; }
  bool is_constant_evaluable() const override { return true; };
  types::type_id type_impl() const override;
  value_category_t value_category_impl() const override;
};

struct binary_operator : derived_visitable<binary_operator, expression> {
  reference<expression> left;
  ast::operator_ operator_;
  reference<expression> right;
  binary_operator(expression& left, ast::operator_&& op, expression& right);

  std ::vector<node*> children() override { return std ::vector<node*>{&left, &operator_, &right}; }
  bool is_constant_evaluable() const override { return true; };
  types::type_id type_impl() const override;
  value_category_t value_category_impl() const override;
};

// using conversion_function = std::function<expression&(expression&)>;

// struct conversion : derived_visitable<conversion, expression> {
//   expression& expr;
//   const conversor* func;
//
//   conversion(expression& exp, const conversor* fn)
//       : expr(exp),
//         func(std::move(fn)) {}
//   types::type_id type_impl() const override { return func->convert_type(expr.type()); };
//   value_category_t value_category_impl() const override {
//     return func->convert_value_category(expr.value_category());
//   };
//   bool is_constant_evaluable() const override { return expr.is_constant_evaluable(); };
//   std::vector<node*> children() override { return {expr}; }
// };

}; // namespace expr
} // namespace cmm::ast
