#pragma once

#include "ast.hpp"
#include "common.hpp"
#include "token.hpp"
#include "types.hpp"
#include "visitor.hpp"

namespace cmm::ast {
namespace terms {
  struct identifier;
  struct literal;
  struct literal;
  struct keyword;
}; // namespace terms
class statement;
namespace expr {
  enum class value_category_t : uint8_t {
    NOT_PROCESSED = 0,
    GLVALUE,
    PRVALUE,
    XVALUE,
    LVALUE,
    RVALUE
  };

  struct semantics {
    bool loaded;
    value_category_t value_category;
    ptr_type original_type;
    bool is_constant_evaluable;
    ptr_type casted_type;
    // const ir::function* fn;
  };

  struct expression : public statement {
    expression();
    NOT_COPYABLE_CLS(expression);
    MOVABLE_CLS(expression);
    // ptr_type type() const { return semantics.original_type; }
    value_category_t category() const { return semantics.value_category; }
    bool is_category(value_category_t) const;
    bool are_semantics_loaded() const { return semantics.loaded; }
    void load_semantics(ptr_type, value_category_t) const;
    virtual cr_type type() const = 0;
    mutable expr::semantics semantics;
  };

  static_assert(std::is_base_of_v<statement, expression>);

  struct identifier : public expression, visitable<terms::identifier, identifier> {
    identifier(ast::terms::identifier&& id);
    cmm::location location() const override;
    cr_type type() const override;
    // FORMAT_DECL_IMPL();
  };

  DERIVE_OK(expression, identifier);

  struct literal : public expression, public terms::literal {
    literal(const token&);
    literal(cmm::location, std::string);
    // FORMAT_DECL_IMPL();
  };
  struct string_literal : visitable<literal, string_literal> {
    string_literal(const token&);
    cr_type type() const override { return type::create_string(value().size()); };
  };
  struct char_literal : visitable<literal, char_literal> {
    char_literal(const token&);
    cr_type type() const override { return type::create_fundamental(type_category_t::char_t); };
  };
  struct sint_literal : visitable<literal, sint_literal> {
    cr_type type() const override { return type::create_fundamental(type_category_t::sint_t); };
    sint_literal(const token&);
  };
  struct uint_literal : visitable<literal, uint_literal> {
    cr_type type() const override { return type::create_fundamental(type_category_t::uint_t); };
    uint_literal(const token&);
  };
  struct false_literal : visitable<literal, false_literal> {
    false_literal(const token&);
    cr_type type() const override { return type::create_fundamental(type_category_t::bool_t); };
  };
  struct true_literal : visitable<literal, true_literal> {
    true_literal(const token&);
    cr_type type() const override { return type::create_fundamental(type_category_t::bool_t); };
  };
  struct float_literal : visitable<literal, float_literal> {
    cr_type type() const override { return type::create_fundamental(type_category_t::float_t); };
    float_literal(const token&);
  };

  struct arguments : public visitable<base_scope<expr::expression*>, arguments> {
    arguments() = default;
    using visitable::visitable;
    // FORMAT_DECL_IMPL();

    std::vector<ptr_type> types() const {
      return vector<expr::expression*>::data() |
             std::views::transform([](const expr::expression* expr) { return &expr->type(); }) |
             std::ranges::to<std::vector>();
    }
  };

  struct call : visitable<expression, call> {
    terms::identifier ident;
    arguments args;

    call(decltype(ident)&& ident_, arguments&& args = {});
    cmm::location location() const override;
    cr_type type() const override;
    // FORMAT_DECL_IMPL();
  };

  struct unary_operator : visitable<expression, unary_operator> {
    expression& expr;
    terms::operator_ operator_;

    unary_operator(expression* expression, terms::operator_&& op);
    cmm::location location() const override;
    cr_type type() const override;
    // FORMAT_DECL_IMPL();
  };

  struct binary_operator : visitable<expression, binary_operator> {
    expression& left;
    expression& right;
    terms::operator_ operator_;

    binary_operator(expression* left, expression* right, terms::operator_&& op);
    cmm::location location() const override;
    cr_type type() const override;
    // FORMAT_DECL_IMPL();
  };

  static_assert(!std::is_copy_constructible_v<binary_operator>);

}; // namespace expr
} // namespace cmm::ast
