#pragma once

#include "common.hpp"
#include "lang.hpp"
#include "macros.hpp"
#include "token.hpp"
#include "traits.hpp"
#include "visitor.hpp"
#include <algorithm>
#include <bits/ranges_algo.h>
#include <initializer_list>
#include <libassert/assert.hpp>
#include <magic_enum/magic_enum.hpp>
#include <magic_enum/magic_enum_format.hpp>
#include <string>
#include <type_traits>
#include <utility>
#include <utils.hpp>

namespace cmm::ast {

using cmm::visitable;

struct base_visitable : visitable<> {

  virtual const base_visitable* get_parent() const { return m_parent; }
  virtual void set_parent(const base_visitable* parent_) const { m_parent = parent_; }

private:
  mutable const base_visitable* m_parent = nullptr;
};

struct node : visitable<base_visitable, node>, public self_allocated, public formattable {
  node(const token& token)
      : self_allocated(token.location()) {}
};

struct statement : visitable<base_visitable, statement>, public allocated, public formattable {
  statement() = default;
};

struct global_statement : public statement {
  using statement::statement;
};
DERIVE_OK(base_visitable, statement);

template <typename...>
inline constexpr bool dependent_false = false;

template <typename T, typename Parent = statement>
struct siblings : public vector<T>, visitable<Parent, siblings<T, Parent>> {
  siblings()
      : vector<T>(),
        visitable<siblings<T>>() {}
  siblings(const std::vector<T>& s)
      : vector<T>(s),
        visitable<siblings<T>>() {}
  siblings(std::vector<T>&& s) noexcept
      : vector<T>(std::move(s)),
        visitable<siblings<T>>() {}
  siblings(std::initializer_list<T> list)
      : vector<T>(list),
        visitable<siblings<T>>() {}

  [[nodiscard]] std::string format() const override {
    return std::format("Array {}({})", cpptrace::demangle(typeid(T).name()), vector<T>::size());
  }
  [[nodiscard]] cmm::location location() const override {
    return std::ranges::fold_left(
        this->cbegin(), this->cend(), location(), [](const cmm::location& acum, const auto& elem) {
          if constexpr (requires { elem.operator->(); }) {
            return acum + elem->location();
          } else if constexpr (std::is_pointer_v<T>) {
            return acum + elem->location();
          } else {
            return acum + elem.location();
          }
        });
  }
};

struct compound : public siblings<statement*> {
  using siblings::location;
  compound(std::vector<statement*>&&);
};

static_assert(Allocated<const compound&>);
static_assert(AllocatedPtr<compound*>);

template <typename T>
concept ZeroParamConstructible = requires {
  { T() } -> std::same_as<T>;
  { T{} } -> std::same_as<T>;
};

DERIVE_OK(statement, compound);

namespace term {
  struct keyword : visitable<node, keyword> {
    token_t kind;
    keyword(const token& token)
        : visitable(token),
          kind(token.type) {}
    FORMAT_DECL_IMPL();
  };

  struct identifier : visitable<node, identifier> {
    std::string value;
    identifier(const token& token)
        : visitable(token),
          value(token.value) {}
    operator cstring() const { return value; }
    FORMAT_DECL_IMPL();
  };

  DERIVE_OK(node, identifier);

  static bool operator==(const identifier& r, const identifier& l) {
    return r.location() == l.location() && r.value == l.value;
  }
  struct literal : visitable<node, literal> {
    std::string value;

    literal(const token& token)
        : visitable(token),
          value(token.value) {}
    FORMAT_DECL_IMPL();
  };

  static_assert(Allocated<const literal&>);
  static_assert(EveryIsAllocated<const literal&, literal, node>);
  DERIVE_OK(node, literal);

  struct operator_ : visitable<node, operator_> {
    operator_t type;
    operator_(const token& token)
        : visitable(token),
          type(token.type.cast<operator_t>()) {}
    operator_(const token& loc, operator_t type_)
        : visitable(loc),
          type(std::move(type_)) {}
    FORMAT_DECL_IMPL();
  };

  struct specifier : visitable<node, specifier> {
    token_t type;
    specifier(const cmm::token& token)
        : visitable(token),
          type(token.type) {}
    FORMAT_DECL_IMPL();
  };

} // namespace term

namespace expr {
  struct expression : visitable<statement, expression> {
    using visitable::visitable;
  };

  static_assert(std::is_base_of_v<statement, expression>);
  struct identifier : visitable<expression, identifier> {
    ast::term::identifier term;
    identifier(ast::term::identifier&& id);
    FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };

  DERIVE_OK(expression, identifier);

  struct literal : visitable<expression, literal> {
    ast::term::literal term;
    type_t type;
    literal(std::string, type_t);
    literal(const token& token);
    FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };

  struct call : visitable<expression, call> {
    using arguments = siblings<expression*>;
    const term::identifier& ident;
    arguments args;

    call(decltype(ident)&& ident_, arguments&& args = {});
    FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };

  struct unary_operator : visitable<expression, unary_operator> {
    expression& expr;
    term::operator_ operator_;

    unary_operator(expression& expression, term::operator_&& op);
    FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };

  struct binary_operator : visitable<expression, binary_operator> {
    expression& left;
    expression& right;
    term::operator_ operator_;

    binary_operator(expression& left, expression& right, term::operator_&& op);
    FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };

#define EXPRESSION_TYPES \
  const ast::expr::binary_operator&, const ast::expr::unary_operator&, const ast::expr::call&, \
      const ast::expr::literal&, const ast::expr::identifier&
}; // namespace expr

namespace decl {
  struct specifiers : public siblings<term::specifier, base_visitable> {};

  struct label : public visitable<statement, label> {
    term::identifier term;
    label(const token&);
    FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };

  static_assert(std::is_move_assignable<label>());

  struct variable : visitable<global_statement, variable> {
    decl::specifiers specifiers;
    const term::identifier* ident; // Can be null, used for function parameters
    expr::expression* init;

    variable(decl::specifiers&&, decltype(ident), decltype(init));
    [[nodiscard]] std::string label() const noexcept {
      return std::format("{}_{}", "variable", ident->value);
    }

    FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };

  struct function : visitable<global_statement, function> {
    using parameters_t = siblings<variable>;

    decl::specifiers specifiers;
    term::identifier ident;
    parameters_t parameters;
    compound* body;

    function(decl::specifiers&&, decltype(ident), decltype(parameters)&&, decltype(body) = nullptr);
    FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };

  static_assert(std::formattable<function, char>);
  static_assert(std::is_polymorphic_v<statement>);
  static_assert(std::is_polymorphic_v<variable>);
  static_assert(std::is_base_of_v<statement, variable>);
  DERIVE_OK(statement, variable);
  DERIVE_OK(statement, label);
  DERIVE_OK(statement, function);
  DERIVE_OK(statement, function);
  DERIVE_OK(statement, variable);
}; // namespace decl
//
namespace selection {
  struct if_ : visitable<statement, if_> {
    term::keyword keyword;
    expr::expression& condition;
    statement* block;
    statement* else_;

    if_(term::keyword, expr::expression&, statement*, statement* = nullptr);
    FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };

}; // namespace selection

namespace iteration {
  template <typename It>
  struct iteration {
  private:
    iteration() = default;

  public:
    virtual ~iteration() = default;
    [[nodiscard]] virtual std::string condition_label() const;
    [[nodiscard]] virtual std::string exit_label() const;
    friend It;
  };

  struct while_ : visitable<statement, while_>, public iteration<while_> {
    term::keyword keyword;
    expr::expression& condition;
    statement* body;
    while_(term::keyword, expr::expression&, statement*);
    FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };

  DERIVE_OK(statement, while_);
  static_assert(!std::is_abstract_v<while_>);

  struct for_ : visitable<statement, for_>, public iteration<for_> {
    term::keyword keyword;
    decl::variable* start;
    expr::expression* condition;
    expr::expression* step;
    statement* body;
    for_(term::keyword k,
         decl::variable* start_,
         expr::expression* condition_,
         expr::expression* step_,
         statement* block);
    FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };

}; // namespace iteration

namespace jump {
  struct goto_ : visitable<statement, goto_> {
    term::identifier term;
    goto_(const token&);
    FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };
  struct break_ : visitable<statement, break_> {
    term::keyword keyword;
    explicit break_(const token& token);
    FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };

  struct continue_ : visitable<statement, continue_> {
    term::keyword keyword;
    explicit continue_(const token& token);
    FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };

  struct return_ : visitable<statement, return_> {
    term::keyword keyword;
    expr::expression* expr;
    return_(term::keyword k, expr::expression* expr_);
    FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };

} // namespace jump
namespace debug {
  // enum class Component : uint8_t { ast, mem, state, tokens };
  // struct printobj : public statement {
  //   Component comp;
  //   printobj(const token& token, Component comp)
  //       : DerivedVisitable(token.location),
  //         comp(comp) {}
  //   [[nodiscard]] std::string format() const override {
  //     return std::string(magic_enum::enum_name<Component>(comp));
  //   }
  // };
} // namespace debug

using program = siblings<ast::statement*>;

DERIVE_OK(siblings<ast::statement*>, program);

#define TERM_TYPES \
  ast::term::literal, ast::term::identifier, ast::term::keyword, ast::term::specifier, \
      ast::term::operator_

#define GLOBAL_TYPES ast::decl::function, ast::decl::variable

#define STATEMENT_TYPES \
  ast::decl::function, ast::decl::variable, ast::compound, ast::decl::label, ast::iteration::for_, \
      ast::iteration::while_, ast::selection::if_, ast::jump::break_, ast::jump::continue_, \
      ast::jump::goto_, ast::jump::return_, ast::expr::expression

#define SIBLING_TYPES \
  ast::expr::call::arguments, ast::decl::function::parameters_t, ast::decl::specifiers

#define NODE_TYPES STATEMENT_TYPES, EXPRESSION_TYPES, TERM_TYPES, SIBLING_TYPES

// struct ast_global_visitor : public revisited::Visitor<GLOBAL_DECL_TYPES>
// {
//   void visit(const ast::decl::variable&) override;
//   void visit(const ast::decl::function&) override;
// };
// struct ast_expression_visitor : public
// revisited::Visitor<EXPRESSION_TYPES>
// {
//   void visit(const ast::expr::call&) override;
//   void visit(const ast::expr::binary_operator&) override;
//   void visit(const ast::expr::unary_operator&) override;
//   void visit(const ast::expr::literal&) override;
//   void visit(const ast::expr::identifier&) override;
// };
// struct ast_statement_visitor : public revisited::Visitor<STATEMENT_TYPES>
// {
//   void visit(const ast::compound& scope) override;
//   void visit(const ast::decl::variable& vardecl) override;
//   void visit(const ast::decl::label& label_) override;
//   void visit(const ast::decl::function& func) override;
//   void visit(const ast::iteration::for_& for_) override;
//   void visit(const ast::iteration::while_& while_) override;
//   void visit(const ast::selection::if_& if_) override;
//   void visit(const ast::jump::break_& break_) override;
//   void visit(const ast::jump::continue_& continue_) override;
//   void visit(const ast::jump::goto_& goto_) override;
//   void visit(const ast::jump::return_& ret) override;
//   void visit(const ast::expr::expression& expr) override;
//   void visit(const ast::debug::printobj& comp) override;
// };
// struct ast_visitor : public revisited::Visitor<NODE_TYPES> {
//   void visit(const ast::expr::call&) override;
//   void visit(const ast::expr::binary_operator&) override;
//   void visit(const ast::expr::unary_operator&) override;
//   void visit(const ast::expr::literal&) override;
//   void visit(const ast::expr::identifier&) override;
//   void visit(const ast::expr::call::arguments&) override;
//   void visit(const ast::decl::function::parameters_t&) override;
//   void visit(const ast::decl::specifiers&) override;
//   void visit(const ast::compound& scope) override;
//   void visit(const ast::decl::variable& vardecl) override;
//   void visit(const ast::decl::label& label_) override;
//   void visit(const ast::decl::function& func) override;
//   void visit(const ast::iteration::for_& for_) override;
//   void visit(const ast::iteration::while_& while_) override;
//   void visit(const ast::selection::if_& if_) override;
//   void visit(const ast::jump::break_& break_) override;
//   void visit(const ast::jump::continue_& continue_) override;
//   void visit(const ast::jump::goto_& goto_) override;
//   void visit(const ast::jump::return_& ret) override;
//   void visit(const ast::expr::expression& expr) override;
//   void visit(const ast::debug::printobj& comp) override;
//   void visit(const ast::program& comp) override;
// };

static_assert(std::is_constructible_v<expr::call::arguments>);
static_assert(Allocated<expr::call::arguments>);
// static_assert(std::is_assignable_v<statement*, jump::goto_*>);
} // namespace cmm::ast

static_assert(std::is_base_of_v<cmm::ast::node, cmm::ast::term::identifier>);
#include "ast.inl"
