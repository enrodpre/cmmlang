#pragma once

#include "common.hpp"
#include "lang.hpp"
#include "token.hpp"
#include "traits.hpp"
#include "visitor.hpp"
#include <algorithm>
#include <bits/ranges_algo.h>
#include <cpptrace/utils.hpp>
#include <initializer_list>
#include <libassert/assert.hpp>
#include <magic_enum/magic_enum.hpp>
#include <magic_enum/magic_enum_format.hpp>
#include <string>
#include <type_traits>
#include <utility>

namespace cmm::ast {

using cmm::visitable;

struct node : virtual visitable<>, public formattable {
  virtual const node* get_parent() const { return m_parent; }
  virtual void set_parent(const node* parent_) const { m_parent = parent_; }

private:
  mutable const node* m_parent = nullptr;
};

struct leaf : public node, public self_allocated {
  leaf(const cmm::location& l)
      : self_allocated(l) {}
};
struct composite : public node, public allocated {
  cmm::location location() const override = 0;
};

struct statement : visitable<composite, statement> {
  statement() = default;
};

struct global_statement : visitable<statement, global_statement> {
  global_statement() = default;
};

DERIVE_OK(composite, statement);

template <typename...>
inline constexpr bool dependent_false = false;

template <typename T, typename Cls, typename Parent = composite>
struct siblings : public vector<T>, public visitable<Parent, Cls> {
private:
  siblings() = default;
  siblings(const std::vector<T>& s)
      : vector<T>(s) {}
  siblings(std::vector<T>&& s) noexcept
      : vector<T>(std::move(s)) {}
  siblings(std::initializer_list<T> list)
      : vector<T>(list) {}

public:
  friend Cls;

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

struct compound : public siblings<statement*, compound, statement> {
  compound(std::vector<statement*>&&);
  using siblings::location;
};

DERIVE_OK(statement, compound);
static_assert(Allocated<const compound&>);
static_assert(AllocatedPtr<compound*>);

template <typename T>
concept ZeroParamConstructible = requires {
  { T() } -> std::same_as<T>;
  { T{} } -> std::same_as<T>;
};

namespace term {
  struct keyword : visitable<leaf, keyword> {
    token_t kind;
    keyword(const token& token)
        : visitable(token.location()),
          kind(token.type) {}
    FORMAT_DECL_IMPL();
  };

  struct identifier : visitable<leaf, identifier> {
    std::string value;
    identifier(const token& token)
        : visitable(token.location()),
          value(token.value) {}
    operator cstring() const { return value; }
    FORMAT_DECL_IMPL();
  };

  DERIVE_OK(node, identifier);

  static bool operator==(const identifier& r, const identifier& l) {
    return r.location() == l.location() && r.value == l.value;
  }
  struct literal : visitable<leaf, literal> {
    std::string value;

    literal(const token& token)
        : visitable(token.location()),
          value(token.value) {}
    FORMAT_DECL_IMPL();
  };

  static_assert(Allocated<const literal&>);
  DERIVE_OK(node, literal);

  struct operator_ : visitable<leaf, operator_> {
    operator_t type;
    operator_(const token& token)
        : visitable(token.location()),
          type(token.type.cast<operator_t>()) {}
    operator_(const token& loc, operator_t type_)
        : visitable(loc.location()),
          type(std::move(type_)) {}
    FORMAT_DECL_IMPL();
  };

  struct specifier : visitable<leaf, specifier> {
    token_t type;
    specifier(const cmm::token& token)
        : visitable(token.location()),
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
    struct arguments : public siblings<expression*, arguments> {
      arguments() = default;
      arguments(std::initializer_list<expression*> l)
          : siblings(l) {}
      arguments(std::vector<expression*>&& v)
          : siblings(std::move(v)) {}
    };
    term::identifier ident;
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
  ast::expr::binary_operator, ast::expr::unary_operator, ast::expr::call, ast::expr::literal, \
      ast::expr::identifier
}; // namespace expr

namespace decl {
  struct specifiers : public siblings<term::specifier, specifiers> {
    specifiers(std::vector<term::specifier>&& v)
        : siblings(std::move(v)) {}
  };

  static_assert(std::is_base_of_v<allocated, specifiers>);
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
    struct parameters_t : public siblings<variable, parameters_t> {
      parameters_t() = default;
      parameters_t(std::vector<variable>&& v)
          : siblings(std::move(v)) {}
    };

    decl::specifiers specifiers;
    term::identifier ident;
    parameters_t parameters;
    const compound* body;

    function(decl::specifiers&&,
             decltype(ident)&&,
             decltype(parameters)&&,
             decltype(body) = nullptr);
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
}; // namespace decl
//
namespace selection {
  struct if_ : visitable<statement, if_> {
    term::keyword keyword;
    expr::expression& condition;
    statement* block;
    statement* else_;

    if_(term::keyword&&, expr::expression&, statement*, statement* = nullptr);
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
    while_(term::keyword&&, expr::expression&, statement*);
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
    for_(term::keyword&& k,
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

struct program : public siblings<ast::global_statement*, program> {
  program(std::vector<ast::global_statement*>&& v)
      : siblings(std::move(v)) {}
};

// DERIVE_OK(siblings<ast::statement*, program>, program);

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
