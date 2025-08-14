#pragma once

#include "ast.hpp"
#include "common.hpp"
#include "token.hpp"
#include "types.hpp"
#include "visitor.hpp"
#include <type_traits>

namespace cmm::ast {

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
    ~expression() override = default;
    using statement::statement;
    value_category_t category() const { return semantics.value_category; }
    bool is_category(value_category_t) const;
    bool are_semantics_loaded() const { return semantics.loaded; }
    void load_semantics(ptr_type, value_category_t) const;
    virtual cr_type type() const { return *semantics.original_type; }
    mutable expr::semantics semantics{};
    AST_LEAF
  };

  static_assert(std::is_base_of_v<statement, expression>);

  struct identifier : visitable<expression, identifier> {
    identifier(ast::identifier&& id);
    // cr_type type() const override;
    const std::string& value() const { return m_term.value(); }
    operator ast::identifier() const { return m_term; }
    AST_LEAF

  private:
    ast::identifier m_term;
  };

  static_assert(std::is_assignable_v<statement*&, expression*>);
  DERIVE_OK(statement, expression);
  DERIVE_OK(expression, identifier);

  struct literal : public expression {
    literal(const token&);
    literal(cmm::location, std::string);
    const std::string& value() const { return m_term.value(); }
    operator ast::literal() const { return m_term; }

    AST_LEAF

  protected:
    ast::literal m_term;
  };

  struct string_literal : visitable<literal, string_literal> {
    string_literal(const token&);
    cr_type type() const override { return cmm::type::create_string(m_term.value().size()); };
    AST_LEAF
  };
  struct char_literal : visitable<literal, char_literal> {
    char_literal(const token&);
    cr_type type() const override {
      return cmm::type::create_fundamental(type_category_t::char_t);
    };
    AST_LEAF
  };
  struct sint_literal : visitable<literal, sint_literal> {
    cr_type type() const override {
      return cmm::type::create_fundamental(type_category_t::sint_t);
    };
    sint_literal(const token&);
    AST_LEAF
  };
  struct uint_literal : visitable<literal, uint_literal> {
    cr_type type() const override {
      return cmm::type::create_fundamental(type_category_t::uint_t);
    };
    uint_literal(const token&);
    AST_LEAF
  };
  struct false_literal : visitable<literal, false_literal> {
    false_literal(const token&);
    cr_type type() const override {
      return cmm::type::create_fundamental(type_category_t::bool_t);
    };
    AST_LEAF
  };
  struct true_literal : visitable<literal, true_literal> {
    true_literal(const token&);
    cr_type type() const override {
      return cmm::type::create_fundamental(type_category_t::bool_t);
    };
    AST_LEAF
  };
  struct float_literal : visitable<literal, float_literal> {
    cr_type type() const override {
      return cmm::type::create_fundamental(type_category_t::float_t);
    };
    float_literal(const token&);
    AST_LEAF
  };

  using arguments = siblings<expr::expression*>;
  struct call : visitable<expression, call> {
    ast::identifier ident;
    arguments args;

    call(decltype(ident)&& ident_, decltype(args)&& args);
    cmm::location location() const override;
    // cr_type type() const override;
    [[nodiscard]] std::vector<ptr_type> types() const {
      return args |
             std::views::transform([](const expr::expression* expr) { return &expr->type(); }) |
             std::ranges::to<std::vector>();
    }
    // FORMAT_DECL_IMPL();
  };

  struct unary_operator : visitable<expression, unary_operator> {
    expression& expr;
    ast::operator_ operator_;

    unary_operator(expression* expression, ast::operator_&& op);
    cmm::location location() const override;
    // cr_type type() const override;
    // FORMAT_DECL_IMPL();
  };

  struct binary_operator : visitable<expression, binary_operator> {
    expression& left;
    expression& right;
    ast::operator_ operator_;

    binary_operator(expression* left, expression* right, ast::operator_&& op);
    cmm::location location() const override;
    // cr_type type() const override { semantics.original_type; }
    // FORMAT_DECL_IMPL();
  };

  struct type_conversion : public expression {
    expression& expr;
    decl::conversion_function& func;
    type_conversion(expression& exp, decl::conversion_function& fn)
        : expr(exp),
          func(fn) {}
  };
  struct implicit_type_conversion : visitable<expression, implicit_type_conversion> {};

}; // namespace expr
} // namespace cmm::ast
