#pragma once

#include "common.hpp"
#include "lang.hpp"
#include "macros.hpp"
#include "revisited/visitor.h"
#include "token.hpp"
#include "traits.hpp"
#include <libassert/assert.hpp>
#include <magic_enum/magic_enum.hpp>
#include <magic_enum/magic_enum_format.hpp>
#include <string>
#include <type_traits>
#include <utility>
#include <utils.hpp>

#define SIMPLE_NODE(REL, NAME, BASE) \
  struct NAME : public REL<NAME, BASE> { \
    using REL::REL; \
  }

#define SIMPLE_VIRTUAL(NAME, BASE) \
  struct NAME : public VirtualVisitable<BASE> { \
    using VirtualVisitable::VirtualVisitable; \
  };

namespace cmm::ast {
template <typename Base, typename Derived>
struct polymorphic_traits {
  static constexpr bool is_base_polymorphic    = std::is_polymorphic_v<Base>;
  static constexpr bool is_derived_polymorphic = std::is_polymorphic_v<Derived>;
  static constexpr bool is_base_of     = std::is_base_of_v<Base, Derived>;
  static constexpr bool is_convertible = std::is_convertible_v<Derived*, Base*>;
  static constexpr bool value = is_base_polymorphic && is_derived_polymorphic &&
                                is_base_of && is_convertible;
};
#define DERIVE_OK(BASE, DERIVED) \
  static_assert(polymorphic_traits<BASE, DERIVED>::value);
using revisited::DerivedVisitable;
using revisited::JoinVisitable;
using revisited::VirtualVisitable;
using revisited::Visitable;
using revisited::VisitableBase;

struct node : public virtual Visitable<node>,
              public formattable,
              public allocated {
  node() = default;
  template <typename... Ts>
  node(Ts&&...);
  ~node() override = default;
  MOVABLE_CLS(node);
  COPYABLE_CLS(node);

  mutable node* parent = nullptr;
  virtual void set_parent(node* parent_) const { parent = parent_; }
};

static_assert(Allocated<node>);

struct statement : public DerivedVisitable<statement, node> {
  using DerivedVisitable::DerivedVisitable;
};

DERIVE_OK(node, statement);

template <typename...>
inline constexpr bool dependent_false = false;

template <typename T, typename NodeType = node>
  requires(std::is_same_v<node, NodeType> ||
           std::is_same_v<NodeType, statement>)
struct siblings : public vector<T>, public VirtualVisitable<NodeType> {
  using vector<T>::vector;
  [[nodiscard]] std::string format() const override {
    return std::format("Array {}({})",
                       cpptrace::demangle(typeid(T).name()),
                       vector<T>::size());
  }
  void set_parent(node* parent_) const override {
    set_parent(parent_);
    for (auto& elem : *this) {
      if constexpr (Ptr<T>) {
        elem->set_parent(this);
      } else {
        elem.get().set_parent(this);
      }
    }
  }
};
struct compound
    : public DerivedVisitable<compound, siblings<statement&, statement>> {
  [[nodiscard]] std::string format() const override {
    return ""; // std::format("{}", this->join(", "));
  }
};

static_assert(std::is_base_of_v<siblings<statement&, statement>, compound>);
static_assert(std::is_base_of_v<statement, compound>);
static_assert(Allocated<const compound&>);
static_assert(AllocatedPtr<compound*>);

template <typename T>
concept ZeroParamConstructible = requires {
  { T() } -> std::same_as<T>;
  { T{} } -> std::same_as<T>;
};

DERIVE_OK(statement, compound);

namespace term {
  struct keyword : public DerivedVisitable<keyword, node> {
    token_t kind;
    keyword(const token& token)
        : DerivedVisitable(token),
          kind(token.type) {}
    FORMAT_DECL_IMPL();
  };

  struct identifier : public DerivedVisitable<identifier, node> {
    std::string value;
    identifier(const token& token)
        : DerivedVisitable(token),
          value(token.value) {}
    operator cstring() const { return value; }
    FORMAT_DECL_IMPL();
  };

  DERIVE_OK(node, identifier);

  static bool operator==(const identifier& r, const identifier& l) {
    return r.location() == l.location() && r.value == l.value;
  }
  struct literal : public DerivedVisitable<literal, node> {
    std::string value;

    literal(const token& token)
        : DerivedVisitable(token),
          value(token.value) {}
    FORMAT_DECL_IMPL();
  };

  static_assert(Allocated<const literal&>);
  static_assert(EveryIsAllocated<const literal&, literal, node>);
  DERIVE_OK(node, literal);

  struct operator_ : public DerivedVisitable<operator_, node> {
    operator_t type;
    operator_(const token& token)
        : DerivedVisitable(token),
          type(token.type.cast<operator_t>()) {}
    operator_(const token& loc, operator_t type_)
        : DerivedVisitable(loc),
          type(std::move(type_)) {}
    FORMAT_DECL_IMPL();
  };

  struct specifier : public DerivedVisitable<specifier, node> {
    token_t type;
    specifier(const token& token)
        : DerivedVisitable(token),
          type(token.type) {}
    FORMAT_DECL_IMPL();
  };

} // namespace term

namespace expr {
  struct expression : public DerivedVisitable<expression, statement> {
    using DerivedVisitable::DerivedVisitable;
    FORMAT_DECL_IMPL();
  };

  static_assert(std::is_base_of_v<node, expression>);
  struct identifier : public DerivedVisitable<identifier, expression> {
    ast::term::identifier term;
    identifier(ast::term::identifier&& id);
    FORMAT_DECL_IMPL();
  };

  DERIVE_OK(expression, identifier);

  struct literal : public DerivedVisitable<literal, expression> {
    ast::term::literal term;
    type_t type;
    literal(std::string, type_t);
    literal(const token& token);
    [[nodiscard]] std::string to_asm() const;
    FORMAT_DECL_IMPL();
  };

  struct call : public DerivedVisitable<call, expression> {
    using arguments = siblings<const expression*>;
    const term::identifier& ident;
    arguments args;

    call(decltype(ident)&& ident_, arguments&& args = {});
    FORMAT_DECL_IMPL();
  };

  struct unary_operator : public DerivedVisitable<unary_operator, expression> {
    expression& expr;
    term::operator_ operator_;

    unary_operator(expression& expression, term::operator_&& op);
    FORMAT_DECL_IMPL();
  };

  struct binary_operator
      : public DerivedVisitable<binary_operator, expression> {
    expression& left;
    expression& right;
    term::operator_ operator_;

    binary_operator(expression& left, expression& right, term::operator_&& op);
    FORMAT_DECL_IMPL();
  };

#define EXPRESSION_TYPES \
  const ast::expr::binary_operator&, const ast::expr::unary_operator&, \
      const ast::expr::call&, const ast::expr::literal&, \
      const ast::expr::identifier&
}; // namespace expr

namespace decl {
  using specifiers = siblings<const term::specifier&>;

  SIMPLE_NODE(DerivedVisitable, declaration, statement);
  SIMPLE_NODE(DerivedVisitable, global_declaration, declaration);

  struct label : public DerivedVisitable<label, declaration> {
    term::identifier term;
    label(const token&);
    FORMAT_DECL_IMPL();
  };
  static_assert(std::is_move_assignable<label>());

  struct variable : public DerivedVisitable<variable, global_declaration> {
    using DerivedVisitable::DerivedVisitable;
    decl::specifiers specifiers;
    const term::identifier* ident; // Can be null, used for function parameters
    expr::expression* init;

    variable(decl::specifiers&&, decltype(ident), decltype(init));
    [[nodiscard]] std::string label() const noexcept {
      return std::format("{}_{}", "variable", ident->value);
    }

    FORMAT_DECL_IMPL();
  };

  struct function : public DerivedVisitable<function, global_declaration> {
    using parameters_t = siblings<const variable&>;

    decl::specifiers specifiers;
    const term::identifier& ident;
    parameters_t parameters;
    compound* body;

    function(decl::specifiers&&,
             decltype(ident),
             decltype(parameters)&&,
             compound* = nullptr);
    FORMAT_DECL_IMPL();
  };

  static_assert(std::formattable<function, char>);
  static_assert(std::is_polymorphic_v<statement>);
  static_assert(std::is_polymorphic_v<variable>);
  static_assert(std::is_base_of_v<statement, variable>);
  DERIVE_OK(statement, variable);
  DERIVE_OK(global_declaration, function);
  DERIVE_OK(statement, variable);
  DERIVE_OK(statement, label);
  DERIVE_OK(statement, function);
  DERIVE_OK(global_declaration, variable);
}; // namespace decl
//
namespace selection {
  struct if_ : public DerivedVisitable<if_, statement> {
    term::keyword keyword;
    expr::expression& condition;
    statement* block;
    statement* else_;

    if_(term::keyword, expr::expression&, statement*, statement* = nullptr);
    FORMAT_DECL_IMPL();
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

  struct while_ : public DerivedVisitable<while_, statement>,
                  // public identifiable_parent<while_>,
                  public iteration<while_> {
    term::keyword keyword;
    expr::expression& condition;
    statement* body;
    while_(term::keyword, expr::expression&, statement*);
    FORMAT_DECL_IMPL();
  };

  DERIVE_OK(statement, while_);
  static_assert(!std::is_abstract_v<while_>);

  struct for_ : public DerivedVisitable<for_, statement>,
                // public identifiable_parent<for_>,
                public iteration<for_> {
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
  };

}; // namespace iteration

namespace jump {
  struct goto_ : public DerivedVisitable<goto_, statement> {
    term::identifier term;
    goto_(const token&);
    FORMAT_DECL_IMPL();
  };
  struct break_ : public DerivedVisitable<break_, statement> {
    term::keyword keyword;
    explicit break_(const token& token);
    FORMAT_DECL_IMPL();
  };

  struct continue_ : public DerivedVisitable<continue_, statement> {
    term::keyword keyword;
    explicit continue_(const token& token);
    FORMAT_DECL_IMPL();
  };

  struct return_ : public DerivedVisitable<return_, statement> {
    term::keyword keyword;
    expr::expression* expr;
    return_(term::keyword k, expr::expression* expr_);
    FORMAT_DECL_IMPL();
  };

} // namespace jump
namespace debug {
  // enum class Component : uint8_t { ast, mem, state, tokens };
  // struct printobj : public DerivedVisitable<printobj, statement> {
  //   Component comp;
  //   printobj(const token& token, Component comp)
  //       : DerivedVisitable(token.location),
  //         comp(comp) {}
  //   [[nodiscard]] std::string format() const override {
  //     return std::string(magic_enum::enum_name<Component>(comp));
  //   }
  // };
} // namespace debug

struct program : public siblings<const decl::global_declaration&> {
  using siblings::siblings;
  [[nodiscard]] std::string format() const override {
    return ""; // std::format("{}", this->join(", "));
  }
};

DERIVE_OK(siblings<const decl::global_declaration&>, program);

#define TERM_TYPES \
  const ast::term::literal&, const ast::term::identifier&, \
      const ast::term::keyword&, const ast::term::specifier&, \
      const ast::term::operator_&

#define GLOBAL_TYPES const ast::decl::function&, const ast::decl::variable&

#define STATEMENT_TYPES \
  const ast::decl::function&, const ast::decl::variable&, \
      const ast::compound&, const ast::decl::label&, \
      const ast::iteration::for_&, const ast::iteration::while_&, \
      const ast::selection::if_&, const ast::jump::break_&, \
      const ast::jump::continue_&, const ast::jump::goto_&, \
      const ast::jump::return_&, const ast::expr::expression&

#define SIBLING_TYPES \
  const ast::expr::call::arguments&, const ast::decl::function::parameters_t&, \
      const ast::decl::specifiers&

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
