#pragma once

#include "ast.hpp"
#include "common.hpp"
#include "token.hpp"
#include "types.hpp"
#include "visitor.hpp"
#include <cstdint>
#include <type_traits>
#include <utility>

namespace cmm::ast {

class statement;

namespace expr {

struct semantic_data {
  semantic_data() = default;
  bool loaded     = false;
  std::optional<value_category_t> value_category;
  bool is_constant_evaluable{};
  ptype original_type;
  ptype casted_type;
};

struct expression : visitable<expression, statement> {
  expression()           = default;
  ~expression() override = default;
  using visitable<expression, statement>::statement;

  semantic_data* semantics() const;
  virtual ptype type() const {
    const auto* sem = semantics();
    return sem->casted_type ? sem->casted_type : sem->original_type;
  }

private:
  mutable expr::semantic_data m_semantics;
};

static_assert(std::is_base_of_v<statement, expression>);

struct identifier : visitable<identifier, expression> {
  identifier(ast::identifier&& id);
  const std::string& value() const { return m_term.value(); }
  operator ast::identifier() const { return m_term; }

private:
  ast::identifier m_term;
};

static_assert(std::is_assignable_v<statement*&, expression*>);
DERIVE_OK(statement, expression);
DERIVE_OK(expression, identifier);

enum class literal_t : uint8_t { CHAR, STRING, SINT, UINT, FALSE, TRUE, FLOAT };
struct literal : visitable<literal, expression> {
  literal_t category;
  literal(const token&, literal_t);
  literal(cmm::location, std::string, literal_t);

  const std::string& value() const { return m_term.value(); }
  operator ast::literal() const { return m_term; }

protected:
  ast::literal m_term;
};

using arguments = siblings<expr::expression*>;

struct call : visitable<call, expression> {
  ast::identifier ident;
  arguments args;

  call(decltype(ident)&& ident_, decltype(args)&& args);
  [[nodiscard]] std::vector<ptype> types() const {
    return args | std::views::transform([](const expr::expression* expr) -> ptype {
             return expr->type();
           }) |
           std::ranges::to<std::vector>();
  }
};

struct unary_operator : visitable<unary_operator, expression> {
  expression& expr;
  ast::operator_ operator_;
  unary_operator(expression* expression, ast::operator_&& op);
};

struct binary_operator : visitable<binary_operator, expression> {
  expression& left;
  expression& right;
  ast::operator_ operator_;
  binary_operator(expression* left, ast::operator_&& op, expression* right);
};

using conversion_function = std::function<expression&(expression&)>;

struct type_conversion : visitable<type_conversion, expression> {
  expression& expr;
  const type_converter func;

  type_conversion(expression& exp, type_converter fn)
      : expr(exp),
        func(std::move(fn)) {}
};

struct implicit_type_conversion : visitable<implicit_type_conversion, type_conversion> {
  using visitable<implicit_type_conversion, type_conversion>::visitable;
};

}; // namespace expr
} // namespace cmm::ast
