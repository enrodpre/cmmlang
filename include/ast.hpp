#pragma once

#include "common.hpp"
#include "lang.hpp"
#include "macros.hpp"
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

#define FORMAT_DECL_IMPL() \
  std::string repr(size_t = 0) const override; \
  std::string format() const override;

using cmm::visitable;

struct node : virtual visitable<>, public formattable {
  node() = default;
  virtual const node* get_parent() const { return m_parent; }
  virtual void set_parent(node* parent_) const { m_parent = parent_; }
  virtual std::optional<cmm::location> location() const = 0;

private:
  mutable node* m_parent = nullptr;
};

std::optional<cmm::location> operator+(const std::optional<cmm::location>& lhs,
                                       const cmm::location& rhs);
std::optional<cmm::location> operator+(const cmm::location& lhs,
                                       const std::optional<cmm::location>& rhs);
std::optional<cmm::location> operator+(const std::optional<cmm::location>& lhs,
                                       const std::optional<cmm::location>& rhs);

struct leaf : public node {
  leaf() = default;
  explicit leaf(cmm::location);

  std::optional<cmm::location> location() const override { return m_location; };

private:
  std::optional<cmm::location> m_location;
};

struct composite : public node {};

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

  [[nodiscard]] std::optional<cmm::location> location() const override {
    return std::ranges::fold_left(this->cbegin(),
                                  this->cend(),
                                  cmm::location(),
                                  [](const cmm::location& acum, const auto& elem) {
                                    if constexpr (requires { elem.operator->(); }) {
                                      return acum + elem->location().value_or(cmm::location());
                                    } else if constexpr (std::is_pointer_v<T>) {
                                      return acum + elem->location().value_or(cmm::location());
                                    } else {
                                      return acum + elem.location().value_or(cmm::location());
                                    }
                                  });
  }
};

struct compound : public siblings<statement*, compound, statement> {
  compound(std::vector<statement*>&&);
  using siblings::location;
  FORMAT_DECL_IMPL();
};

DERIVE_OK(statement, compound);

template <typename T>
concept ZeroParamConstructible = requires {
  { T() } -> std::same_as<T>;
  { T{} } -> std::same_as<T>;
};

namespace term {
  template <typename T>
    requires(Formattable<T>)
  struct lazy_format {
    lazy_format(const T& t)
        : m_obj(t),
          m_func([](const T& t) { return t.format(); }) {}

    cr_string value(const T& t) const {
      if (!m_value.has_value()) {
        m_value = m_func(t);
      }
      return m_value.value();
    }

  private:
    mutable std::optional<std::string> m_value;
    std::reference_wrapper<const T> m_obj;
    std::function<std::string(const T&)> m_func;
  };
  struct term : visitable<leaf, term> {
    term() = default;
    term(const token& tok)
        : visitable(tok.location()) {}
    virtual const std::string& value() const = 0;
    std::string repr(size_t n = 0) const override {
      return std::format("{}{}", std::string(n * 2, ' '), value());
    };
    std::string format() const override { return value(); }
  };
  struct keyword : visitable<term, keyword> {
    token_t kind;
    keyword(const token& token)
        : visitable(token),
          kind(token.type),
          m_value(kind) {}
    const std::string& value() const override { return m_value.value(kind); }

  private:
    lazy_format<token_t> m_value;
  };

  struct identifier : visitable<term, identifier> {
    identifier(std::string v)
        : m_value(std::move(v)) {}
    identifier(const token& token)
        : visitable(token),
          m_value(token.value) {}
    operator cstring() const { return m_value; }
    const std::string& value() const override { return m_value; }

  private:
    std::string m_value;
  };

  static_assert(maybe_allocated<const identifier&>);
  static_assert(std::formattable<identifier, char>);
  DERIVE_OK(node, identifier);

  static bool operator==(const identifier& r, const identifier& l) {
    return r.location() == l.location() && r.value() == l.value();
  }
  struct literal : visitable<term, literal> {
    literal(const token& token)
        : visitable(token),
          m_value(token.value) {}
    const std::string& value() const override { return m_value; }

  private:
    std::string m_value;
  };

  DERIVE_OK(node, literal);

  struct operator_ : visitable<term, operator_> {
    operator_t type;
    operator_(const token& loc)
        : visitable(loc),
          type(loc.type.cast<operator_t>()),
          m_value(type) {}
    operator_(const token& loc, operator_t type_)
        : visitable(loc),
          type(std::move(type_)),
          m_value(type) {}
    const std::string& value() const override { return m_value.value(type); }

  private:
    lazy_format<operator_t> m_value;
  };

  struct specifier : visitable<term, specifier> {
    token_t type;
    specifier(const token& token)
        : visitable(token),
          type(token.type),
          m_value(type) {}
    const std::string& value() const override { return m_value.value(type); }

  private:
    lazy_format<token_t> m_value;
  };

} // namespace term

namespace expr {
  struct compilation_unit;

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
  };

  struct expression : visitable<statement, expression, expression> {
    expression();
    NOT_COPYABLE_CLS(expression);
    MOVABLE_CLS(expression);
    ptr_type type() const { return semantics.original_type; }
    value_category_t category() const { return semantics.value_category; }
    bool is_category(value_category_t) const;
    bool are_semantics_loaded() const { return semantics.loaded; }
    void load_semantics(ptr_type, value_category_t) const;

    mutable expr::semantics semantics;
  };

  static_assert(std::is_base_of_v<statement, expression>);

  struct identifier : visitable<expression, identifier> {
    ast::term::identifier term;
    identifier(ast::term::identifier&& id);
    std::optional<cmm::location> location() const override;
    FORMAT_DECL_IMPL();
    static_assert(!std::is_pointer_v<decltype(term)>);
  };

  DERIVE_OK(expression, identifier);

  struct literal : visitable<expression, literal> {
    ast::term::literal term;
    literal(std::string, type_category_t);
    literal(const token& token);
    std::optional<cmm::location> location() const override;
    FORMAT_DECL_IMPL();
    ptr_type type;
  };

  struct call : visitable<expression, call> {
    struct arguments : public siblings<expression*, arguments> {
      arguments() = default;
      arguments(std::initializer_list<expression*> l)
          : siblings(l) {}
      arguments(std::vector<expression*>&& v)
          : siblings(std::move(v)) {}
      FORMAT_DECL_IMPL();

      std::vector<ptr_type> types() const {
        return *this | std::views::transform([this](const auto& expr) -> ptr_type {
          return expr->type();
        }) | std::ranges::to<std::vector>();
      }
    };
    term::identifier ident;
    arguments args;

    call(decltype(ident)&& ident_, arguments&& args = {});
    std::optional<cmm::location> location() const override;
    FORMAT_DECL_IMPL();
  };

  struct unary_operator : visitable<expression, unary_operator> {
    expression& expr;
    term::operator_ operator_;

    unary_operator(expression* expression, term::operator_&& op);
    std::optional<cmm::location> location() const override;
    FORMAT_DECL_IMPL();
  };

  struct binary_operator : visitable<expression, binary_operator> {
    expression& left;
    expression& right;
    term::operator_ operator_;

    binary_operator(expression* left, expression* right, term::operator_&& op);
    std::optional<cmm::location> location() const override;
    FORMAT_DECL_IMPL();
  };

  static_assert(!std::is_copy_constructible_v<binary_operator>);

#define EXPRESSION_TYPES \
  ast::expr::binary_operator, ast::expr::unary_operator, ast::expr::call, ast::expr::literal, \
      ast::expr::identifier
}; // namespace expr

namespace decl {
  struct specifiers : public siblings<term::specifier, specifiers> {
    specifiers(std::vector<term::specifier>&& v)
        : siblings(std::move(v)) {}
    FORMAT_DECL_IMPL();
    cr_type parse_type() const;
    storage_t parse_storage() const;
    linkage_t parse_linkage() const;
  };

  struct declaration {};
  struct label : public visitable<statement, label>, declaration {
    term::identifier term;
    label(const token&);
    FORMAT_DECL_IMPL();
    std::optional<cmm::location> location() const override;
  };

  static_assert(std::is_copy_assignable<label>());
  static_assert(std::is_copy_constructible<label>());
  static_assert(std::is_move_constructible<label>());
  static_assert(std::is_move_assignable<label>());

  struct variable : visitable<global_statement, variable>, declaration {
    decl::specifiers specifiers;
    term::identifier* ident; // Can be null, used for function parameters
    expr::expression* init;

    variable(decl::specifiers&&, decltype(ident), decltype(init));
    [[nodiscard]] std::string label() const noexcept {
      return std::format("{}_{}", "variable", ident->value());
    }

    FORMAT_DECL_IMPL();
    std::optional<cmm::location> location() const override;
  };
  static_assert(std::is_copy_assignable<variable>());
  static_assert(std::is_copy_constructible<variable>());
  static_assert(std::is_move_constructible<term::identifier>());
  static_assert(std::is_move_assignable<term::identifier>());

  struct function : visitable<global_statement, function>, declaration {
    struct parameters_t : public siblings<variable, parameters_t> {
      parameters_t() = default;
      parameters_t(std::vector<variable>&& v)
          : siblings(std::move(v)) {}
      FORMAT_DECL_IMPL();
    };
    static_assert(std::is_object_v<parameters_t>);

    decl::specifiers specifiers;
    term::identifier ident;
    parameters_t parameters;
    compound* body;

    function(decl::specifiers&&,
             decltype(ident)&&,
             decltype(parameters)&&,
             decltype(body) = nullptr);
    FORMAT_DECL_IMPL();
    std::optional<cmm::location> location() const override;
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
    std::optional<cmm::location> location() const override;
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
    std::optional<cmm::location> location() const override;
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
    std::optional<cmm::location> location() const override;
  };

}; // namespace iteration

namespace jump {
  struct goto_ : visitable<statement, goto_> {
    term::identifier term;
    goto_(const token&);
    FORMAT_DECL_IMPL();
    std::optional<cmm::location> location() const override;
  };
  struct break_ : visitable<statement, break_> {
    term::keyword keyword;
    explicit break_(const token& token);
    FORMAT_DECL_IMPL();
    std::optional<cmm::location> location() const override;
  };

  struct continue_ : visitable<statement, continue_> {
    term::keyword keyword;
    explicit continue_(const token& token);
    FORMAT_DECL_IMPL();
    std::optional<cmm::location> location() const override;
  };

  struct return_ : visitable<statement, return_> {
    term::keyword keyword;
    expr::expression* expr;
    return_(term::keyword k, expr::expression* expr_);
    FORMAT_DECL_IMPL();
    std::optional<cmm::location> location() const override;
  };

} // namespace jump

struct program : public siblings<ast::global_statement*, program> {
  program(std::vector<ast::global_statement*>&& v)
      : siblings(std::move(v)) {}
  FORMAT_DECL_IMPL();
};

#define TRACE_VISITOR(OBJ) \
  REGISTER_TRACE("{} visited {}", \
                 cpptrace::demangle(typeid(this).name()), \
                 cpptrace::demangle(typeid(OBJ).name()))
#define TERM_TYPES \
  ast::term::literal, ast::term::identifier, ast::term::keyword, ast::term::specifier, \
      ast::term::operator_

#define GLOBAL_TYPES ast::decl::function, ast::decl::variable

#define STATEMENT_TYPES \
  ast::compound, ast::decl::label, ast::iteration::for_, ast::iteration::while_, \
      ast::selection::if_, ast::jump::break_, ast::jump::continue_, ast::jump::goto_, \
      ast::jump::return_

#define CHILDREN_TYPES \
  ast::expr::call::arguments, ast::decl::function::parameters_t, ast::decl::specifiers, ast::program

#define NODE_TYPES STATEMENT_TYPES, EXPRESSION_TYPES, TERM_TYPES, CHILDREN_TYPES, GLOBAL_TYPES

struct ast_visitor : cmm::ref_visitor<NODE_TYPES> {
  void visit(ast::term::operator_& c) override { TRACE_VISITOR(c); }
  void visit(ast::term::literal& c) override { TRACE_VISITOR(c); }
  void visit(ast::term::keyword& c) override { TRACE_VISITOR(c); }
  void visit(ast::term::specifier& c) override { TRACE_VISITOR(c); }
  void visit(ast::term::identifier& c) override { TRACE_VISITOR(c); }
  void visit(ast::expr::call::arguments& a) override {
    TRACE_VISITOR(a);
    for (auto&& arg : a) {
      arg->accept(*this);
    }
  };
  void visit(ast::decl::function::parameters_t& p) override {
    TRACE_VISITOR(p);
    for (auto&& par : p) {
      par.accept(*this);
    }
  };
  void visit(ast::decl::specifiers& s) override {
    TRACE_VISITOR(s);
    for (auto&& par : s) {
      par.accept(*this);
    }
  };
  void visit(ast ::expr ::identifier& c) override {
    TRACE_VISITOR(c);
    c.term.accept(*this);
  };
  void visit(ast ::expr ::literal& c) override {
    TRACE_VISITOR(c);
    c.term.accept(*this);
  };
  void visit(ast ::expr ::unary_operator& c) override {
    TRACE_VISITOR(c);
    c.operator_.accept(*this);
    c.expr.accept(*this);
  };
  void visit(ast ::expr ::binary_operator& c) override {
    TRACE_VISITOR(c);
    c.operator_.accept(*this);
    c.left.accept(*this);
    c.right.accept(*this);
  };
  void visit(ast ::expr ::call& c) override {
    TRACE_VISITOR(c);
    c.ident.accept(*this);
    c.args.accept(*this);
  };
  void visit(ast ::compound& c) override {
    TRACE_VISITOR(c);
    for (auto&& d : c) {
      d->accept(*this);
    }
  };
  void visit(ast ::decl ::variable& c) override {
    TRACE_VISITOR(c);
    if (auto* ident = c.ident) {
      ident->accept(*this);
    }
    c.specifiers.accept(*this);
    if (auto* init = c.init) {
      init->accept(*this);
    }
  };
  void visit(ast ::decl ::function& c) override {
    TRACE_VISITOR(c);
    c.specifiers.accept(*this);
    c.ident.accept(*this);
    c.parameters.accept(*this);
    if (auto* body = c.body) {
      body->accept(*this);
    }
  };
  void visit(ast ::decl ::label& c) override {
    TRACE_VISITOR(c);
    c.term.accept(*this);
  };
  void visit(ast ::iteration ::while_& c) override {
    TRACE_VISITOR(c);
    c.condition.accept(*this);
    if (auto* body = c.body) {
      body->accept(*this);
    }
  };
  void visit(ast ::iteration ::for_& c) override {
    TRACE_VISITOR(c);
    c.start->accept(*this);
    if (auto* cond = c.condition) {
      cond->accept(*this);
    }
    if (auto* step = c.step) {
      step->accept(*this);
    }
    if (auto* body = c.body) {
      body->accept(*this);
    }
  };
  void visit(ast ::selection ::if_& c) override {
    TRACE_VISITOR(c);
    c.condition.accept(*this);
    if (auto* block = c.block) {
      block->accept(*this);
    }
    if (auto* else_ = c.else_) {
      else_->accept(*this);
    }
  };
  void visit(ast ::jump ::goto_& c) override {
    TRACE_VISITOR(c);
    c.term.accept(*this);
  };
  void visit(ast ::jump ::return_& c) override {
    TRACE_VISITOR(c);
    if (auto* expr = c.expr) {
      expr->accept(*this);
    }
  }
  void visit(ast::jump::continue_& c) override { TRACE_VISITOR(c); }
  void visit(ast::jump::break_& c) override { TRACE_VISITOR(c); }
  void visit(ast::program& p) override {
    TRACE_VISITOR(p);
    for (auto&& d : p) {
      p.accept(*this);
    }
  }
};
struct const_ast_visitor : cmm::cref_visitor<NODE_TYPES> {
  void visit(const ast::term::operator_& c) override { TRACE_VISITOR(c); }
  void visit(const ast::term::literal& c) override { TRACE_VISITOR(c); }
  void visit(const ast::term::keyword& c) override { TRACE_VISITOR(c); }
  void visit(const ast::term::specifier& c) override { TRACE_VISITOR(c); }
  void visit(const ast::term::identifier& c) override { TRACE_VISITOR(c); }
  void visit(const ast::expr::call::arguments& a) override {
    TRACE_VISITOR(a);
    for (auto&& arg : a) {
      arg->accept(*this);
    }
  };
  void visit(const ast::decl::function::parameters_t& p) override {
    TRACE_VISITOR(p);
    for (auto&& par : p) {
      par.accept(*this);
    }
  };
  void visit(const ast::decl::specifiers& s) override {
    TRACE_VISITOR(s);
    for (auto&& par : s) {
      par.accept(*this);
    }
  };
  void visit(const ast ::expr ::identifier& c) override {
    TRACE_VISITOR(c);
    c.term.accept(*this);
  };
  void visit(const ast ::expr ::literal& c) override {
    TRACE_VISITOR(c);
    c.term.accept(*this);
  };
  void visit(const ast ::expr ::unary_operator& c) override {
    TRACE_VISITOR(c);
    c.operator_.accept(*this);
    c.expr.accept(*this);
  };
  void visit(const ast ::expr ::binary_operator& c) override {
    TRACE_VISITOR(c);
    c.operator_.accept(*this);
    c.left.accept(*this);
    c.right.accept(*this);
  };
  void visit(const ast ::expr ::call& c) override {
    TRACE_VISITOR(c);
    c.ident.accept(*this);
    c.args.accept(*this);
  };
  void visit(const ast ::compound& c) override {
    TRACE_VISITOR(c);
    for (auto&& d : c) {
      d->accept(*this);
    }
  };
  void visit(const ast ::decl ::variable& c) override {
    TRACE_VISITOR(c);
    if (auto* ident = c.ident) {
      ident->accept(*this);
    }
    c.specifiers.accept(*this);
    if (auto* init = c.init) {
      init->accept(*this);
    }
  };
  void visit(const ast ::decl ::function& c) override {
    TRACE_VISITOR(c);
    c.specifiers.accept(*this);
    c.ident.accept(*this);
    c.parameters.accept(*this);
    if (auto* body = c.body) {
      body->accept(*this);
    }
  };
  void visit(const ast ::decl ::label& c) override {
    TRACE_VISITOR(c);
    c.term.accept(*this);
  };
  void visit(const ast ::iteration ::while_& c) override {
    TRACE_VISITOR(c);
    c.condition.accept(*this);
    if (auto* body = c.body) {
      body->accept(*this);
    }
  };
  void visit(const ast ::iteration ::for_& c) override {
    TRACE_VISITOR(c);
    c.start->accept(*this);
    if (auto* cond = c.condition) {
      cond->accept(*this);
    }
    if (auto* step = c.step) {
      step->accept(*this);
    }
    if (auto* body = c.body) {
      body->accept(*this);
    }
  };
  void visit(const ast ::selection ::if_& c) override {
    TRACE_VISITOR(c);
    c.condition.accept(*this);
    if (auto* block = c.block) {
      block->accept(*this);
    }
    if (auto* else_ = c.else_) {
      else_->accept(*this);
    }
  };
  void visit(const ast ::jump ::goto_& c) override {
    TRACE_VISITOR(c);
    c.term.accept(*this);
  };
  void visit(const ast ::jump ::return_& c) override {
    TRACE_VISITOR(c);
    if (auto* expr = c.expr) {
      expr->accept(*this);
    }
  }
  void visit(const ast::jump::continue_& c) override { TRACE_VISITOR(c); }
  void visit(const ast::jump::break_& c) override { TRACE_VISITOR(c); }
  void visit(const ast::program& p) override {
    TRACE_VISITOR(p);
    for (auto&& d : p) {
      p.accept(*this);
    }
  }
};
static_assert(std::is_constructible_v<expr::call::arguments>);
} // namespace cmm::ast

static_assert(std::is_base_of_v<cmm::ast::node, cmm::ast::term::identifier>);
#include "ast.inl"
