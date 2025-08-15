#pragma once

#include "ast.hpp"
#include "common.hpp"
#include "token.hpp"
#include "types.hpp"
#include "visitor.hpp"
#include <cstdint>
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
    ptr_type contextually_castable;
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

  struct identifier : visitable<identifier, expression> {
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

  enum class literal_t : uint8_t { CHAR, STRING, SINT, UINT, FALSE, TRUE, FLOAT };
  struct literal : visitable<literal, expression> {
    literal_t category;
    literal(const token&, literal_t);
    literal(cmm::location, std::string, literal_t);
    const std::string& value() const { return m_term.value(); }
    operator ast::literal() const { return m_term; }
    cr_type type() const override {
      switch (category) {
        case literal_t::CHAR:
          return cmm::type::create_fundamental(type_category_t::char_t);
        case literal_t::STRING:
          return cmm::type::create_string(m_term.value().size());
        case literal_t::SINT:
          return cmm::type::create_fundamental(type_category_t::sint_t);
        case literal_t::UINT:
          return cmm::type::create_fundamental(type_category_t::uint_t);
        case literal_t::FALSE:
        case literal_t::TRUE:
          return cmm::type::create_fundamental(type_category_t::bool_t);
        case literal_t::FLOAT:
          return cmm::type::create_fundamental(type_category_t::float_t);
          break;
      }
      __builtin_unreachable();
    }

    AST_LEAF

  protected:
    ast::literal m_term;
  };

  using arguments = siblings<expr::expression*>;
  struct call : visitable<call, expression> {
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

  struct unary_operator : visitable<unary_operator, expression> {
    expression& expr;
    ast::operator_ operator_;

    unary_operator(expression* expression, ast::operator_&& op);
    cmm::location location() const override;
    // cr_type type() const override;
    // FORMAT_DECL_IMPL();
  };

  struct binary_operator : visitable<binary_operator, expression> {
    expression& left;
    expression& right;
    ast::operator_ operator_;

    binary_operator(expression* left, expression* right, ast::operator_&& op);
    cmm::location location() const override;
    // cr_type type() const override { semantics.original_type; }
    // FORMAT_DECL_IMPL();
  };

  using conversion_function = std::function<expression&(expression&)>;

  struct type_conversion : visitable<type_conversion, expression> {
    expression& expr;
    const conversions::converter_t func;
    type_conversion(expression& exp, const decltype(func)& fn)
        : expr(exp),
          func(fn) {}
  };
  struct implicit_type_conversion : visitable<implicit_type_conversion, type_conversion> {
    using visitable<implicit_type_conversion, type_conversion>::visitable;
  };

}; // namespace expr
} // namespace cmm::ast
