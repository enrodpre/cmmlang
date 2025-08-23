#pragma once

#include "common.hpp"
#include "lang.hpp"
#include "macros.hpp"
#include "revisited/visitor.h"
#include "token.hpp"
#include "traits.hpp"
#include "types.hpp"
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

namespace cmm::assembly {
struct operand;
}

namespace cmm::ir {
struct compilation_unit;
}

namespace cmm::ast {

template <typename B, typename A>
using visitable = visitable<B, A>;

struct leaf : visitable<leaf, node> {
  leaf() = default;
  leaf(std::optional<cmm::location>);
  leaf(const token&);

  std::optional<cmm::location> location() const final { return m_location; }

protected:
  void add(node* n) {
    if (n != nullptr) {
      n->set_parent(this);
    }
  }

private:
  std::optional<cmm::location> m_location;
};

#define term_properties(TYPE)                                                \
  operator TYPE() const { return value(); }                                  \
  [[nodiscard]] const TYPE& value() const { return m_value; }                \
  std::string string() const override { return std::format("{}", m_value); }

struct keyword : visitable<keyword, leaf> {
  using visitable<keyword, leaf>::visitable;
  term_properties(keyword_t);

private:
  keyword_t m_value;
};

static_assert(std::is_base_of_v<leaf, keyword>);
static_assert(std::is_base_of_v<cmm::ast::node, keyword>);

struct literal : visitable<literal, leaf> {
  using visitable<literal, leaf>::visitable;

  literal(cmm::location l, std::string s)
      : visitable<literal, leaf>(std::move(l)),
        m_value(std::move(s)) {}

  term_properties(std::string);

private:
  std::string m_value;
};

struct operator_ : visitable<operator_, leaf> {
  operator_(const token& t)
      : visitable<operator_, leaf>(t),
        m_value(token_data(t.type).cast<operator_t>()) {}

  operator_(const token& t, operator_t op)
      : visitable<operator_, leaf>(t),
        m_value(op) {}

  std::string string() const override {
    return std::format("operator{}", operator_data(m_value).repr);
  }

  operator operator_t() const { return value(); }
  [[nodiscard]] const operator_t& value() const { return m_value; }

private:
  operator_t m_value;
};

template <typename T>
struct specifier : visitable<specifier<T>, leaf> {
  using parent_t = visitable<specifier<T>, leaf>;
  using parent_t::parent_t;
  specifier() = default;

  specifier(const token& x)
      : parent_t(x) {}

  friend T;
};

struct storage : visitable<storage, specifier<storage_t>> {
  storage() = default;

  storage(const token& t, storage_t l)
      : visitable<storage, specifier<storage_t>>(t),
        m_value(l) {}

  term_properties(storage_t);

private:
  storage_t m_value{};
};

struct linkage : visitable<linkage, specifier<linkage_t>> {
  linkage() = default;

  linkage(const token& t, linkage_t l)
      : visitable<linkage, specifier<linkage_t>>(t),
        m_value(l) {}

  term_properties(linkage_t);

private:
  linkage_t m_value{};
};

struct type : visitable<type, specifier<ptype>> {
  type(ptype t)
      : m_value(std::move(t)) {}

  term_properties(ptype);

private:
  ptype m_value;
};

struct identifier : visitable<identifier, leaf> {
  identifier() = default;

  identifier(const token& t)
      : visitable<identifier, leaf>(t),
        m_value(t.value) {}

  identifier(std::string name)
      : m_value(std::move(name)) {}

  identifier(const operator_& op)
      : visitable<identifier, leaf>(op.location()),
        m_value(std::format("operator{}", op.value())) {}

  bool operator==(const identifier& other) const { return m_value == other.m_value; }
  term_properties(std::string);
  operator cstring() const { return value(); }

private:
  std::string m_value;
};

static inline identifier anonymous_identifier() { return {"anonymous"}; }

struct statement : visitable<statement, composite> {
  using visitable<statement, composite>::visitable;
  statement(statement&&) noexcept = default;

  statement& operator=(statement&& other) noexcept {
    composite::operator=(std::move(other));
    return *this;
  }

  COPYABLE_CLS(statement);
};

struct empty_statement_t : visitable<empty_statement_t, statement> {
  empty_statement_t() = default;

  std::string string() const override { return "empty_statement"; }
};

static_assert(std::is_base_of_v<statement, empty_statement_t>);
inline empty_statement_t _empty_statement{};                   // NOLINT
inline empty_statement_t* empty_statement = &_empty_statement; // NOLINT

namespace expr {
struct expression;
struct literal;
struct identifier;
struct unary_operator;
struct call;
struct binary_operator;
struct implicit_type_conversion;
}; // namespace expr

template <typename T>
concept has_type = requires(T t) {
  { t.type() } -> std::same_as<cmm::location>;
};

template <has_type T>
constexpr static auto EXTRACT_TYPE = [](const T& t) { return t.type(); };

using mangled_key                  = std::string;

struct anonymous_declaration : visitable<anonymous_declaration, statement> {
  anonymous_declaration() = default;
  using visitable<anonymous_declaration, statement>::DerivedVisitable;
};

struct declaration : visitable<declaration, anonymous_declaration> {
  using visitable<declaration, anonymous_declaration>::anonymous_declaration;
  identifier ident;

  declaration() = delete;
  declaration(decltype(ident) t)
      : ident(std::move(t)) {}
  std::string string() const final { return ident.string(); }
};

namespace decl {
struct function;
struct variable;
struct label;
}; // namespace decl

using variable_store_value = struct variable_store
    : public hashmap<mangled_key, std::pair<const decl::variable*, assembly::operand*>> {
  using key_type   = mangled_key;
  using value_type = std::pair<const decl::variable*, assembly::operand*>;
  variable_store() = default;
  void insert(const decl::variable*, assembly::operand*);
};

// dynamic_cast<node*>(elem); };

template <typename T>
struct siblings : public vector<T>, public visitable<siblings<T>, composite> {
  siblings() = default;

  siblings(std::initializer_list<T> init)
      : vector<T>(init) {}

  siblings(siblings<T>&&)                 = default;
  siblings& operator=(siblings<T>&&)      = default;
  siblings(const siblings<T>&)            = default;
  siblings& operator=(const siblings<T>&) = default;

  siblings(std::vector<T>&& s) noexcept
      : vector<T>(std::move(s)) {}

  siblings(const std::vector<T>& s) noexcept
      : vector<T>(std::move(s)) {}

  [[nodiscard]] std::optional<cmm::location> location() const final {
    return std::ranges::fold_left_first(*this | std::views::transform([](const T& n) {
      if constexpr (std::is_pointer_v<std::remove_cvref_t<T>>) {
        return n->location();
      } else {
        return n.location();
      }
    }) | FILTER(opt_has_value) | TRANSFORM(opt_to_value),
                                        std::plus<cmm::location>{});
  }

  [[nodiscard]] std::string string() const override {
    return cpptrace::demangle(typeid(this).name());
  }
};

namespace expr {
using arguments = siblings<expr::expression*>;
}

DERIVE_OK(composite, statement);

using global_declarations = siblings<declaration*>;
using statements          = siblings<statement*>;

struct scope : visitable<scope, anonymous_declaration> {
  using visitable<scope, anonymous_declaration>::DerivedVisitable;
  [[nodiscard]] virtual bool is_declared(const ast::identifier&) const;
  [[nodiscard]] virtual const variable_store::value_type& get_variable(
      const ast::identifier&) const;
  [[nodiscard]] const ast::decl::variable* get_variable_declaration(const ast::identifier&) const;
  [[nodiscard]] assembly::operand* get_variable_address(const ast::identifier&) const;
  variable_store variables;

  friend struct translation_unit;

protected:
  virtual assembly::operand* declare_variable(ast::decl::variable*);
};

template <typename T>
struct executable_scope : scope, siblings<T> {};

using label_store = std::unordered_map<std::string, const decl::label*>;

namespace decl {
struct block : visitable<block, scope> {
  block(const statements& s)
      : stmts(s) {
    add(s.data());
  }

  void declare_label(const decl::label*);
  statements stmts;
  label_store labels;
};

struct specifiers : visitable<specifiers, composite> {
  ast::type type;
  ast::linkage linkage;
  ast::storage storage;
  specifiers(ast::type&&, ast::linkage&&, ast::storage&&);

  specifiers(ptype t, decltype(linkage) l = {}, decltype(storage) s = {})
      : type(std::move(t)),
        linkage(std::move(l)),
        storage(std::move(s)) {
    add_all(&type, &linkage, &storage);
  }
};

struct rank : visitable<rank, composite> {
  ast::operator_ open;
  expr::expression* number;
  ast::operator_ close;

  rank(const token&, const token&);
  rank(const token&, decltype(number), const token&);
};

struct label : public visitable<label, declaration> {
  label(const token&);
};

struct variable : visitable<variable, declaration> {
  specifiers specs;
  decl::rank* rank;
  expr::expression* init;

  variable(specifiers&&, decltype(rank), identifier, decltype(init));
  variable(ptype, decltype(ident), decltype(init));

  std::string repr() const override { return std::format("variable_{}", string()); }
};

struct function : visitable<function, declaration>, public callable {
  using identifier_type = identifier;
  struct definition;

  specifiers specs;
  siblings<variable> params;
  definition* body;

  function(decltype(specs)&&, decltype(ident)&&, decltype(params)&&, decltype(body));

  function(operator_& name, ptype ret, decltype(params)& p, decltype(body) b)
      : visitable<function, declaration>(name),
        specs(std::move(ret)),
        params(p),
        body(b) {
    add_all(&specs, &ident, &params, body);
  }

  callable_contract contract() const override;
  std::vector<parameter> parameters() const override {
    return params | std::views::transform([](const variable& param) -> parameter {
             return {param.specs.type.value(), param.ident.value(), param.init, &param};
           }) |
           std::ranges::to<std::vector>();
  }
};

struct function::definition : visitable<definition, block> {
  definition(const siblings<statement*>& s)
      : visitable<definition, block>(s) {}

  struct {
    [[nodiscard]] size_t size() const { return stack_size; };
    void push() { stack_size++; };
    void pop(size_t times = 1) { stack_size -= times; }

  private:
    size_t stack_size = 0;
  } local_stack;

  block* active_scope() { return local_scopes.top(); }
  const block* active_scope() const { return local_scopes.top(); }
  bool is_declared(const identifier& ident) const override;
  void create_scope(block&) noexcept;
  [[nodiscard]] const variable_store::value_type& get_variable(
      const identifier& ident) const override;
  assembly::operand* declare_parameter(const ast::decl::variable&, assembly::operand*);
  size_t destroy_scope() noexcept;
  void clear() noexcept;
  label_store labels;
  stack<block*> local_scopes;
};

static_assert(std::is_base_of_v<block, function::definition>);
static_assert(std::is_polymorphic_v<statement>);
static_assert(std::is_polymorphic_v<variable>);
static_assert(std::is_base_of_v<statement, variable>);
DERIVE_OK(statement, variable);
DERIVE_OK(statement, label);
DERIVE_OK(statement, function);

struct conversion_function : public function {
  enum class conversion_type_t : uint8_t { IMPLICIT, EXPLICIT };
  conversion_type_t type;
  conversion_function(const decltype(body)&);
  [[nodiscard]] virtual bool is_convertible(ptype) const noexcept = 0;
  [[nodiscard]] virtual ptype to(crptype) const noexcept          = 0;
  [[nodiscard]] virtual assembly::operand* operator()(assembly::operand*) const noexcept;
  [[nodiscard]] bool is_implicit() const { return type == conversion_type_t::IMPLICIT; };
  [[nodiscard]] bool is_explicit() const { return type == conversion_type_t::EXPLICIT; };
};

}; // namespace decl

class conversion_store {
public:
  // using key_type       = mangled_key;
  // using value_type     = std::unordered_map<key_type,
  // decl::direct_conversion>; using container_type =
  // std::unordered_map<key_type, value_type>;

  // conversion_store() = default;
  // [[nodiscard]] bool is_convertible(ptype, ptype) const;
  // [[nodiscard]] std::vector<ptype> get_convertible_types(ptype) const;
  // [[nodiscard]] std::vector<const decl::conversion_function*>
  // get_conversions(ptype) const; void emplace_direct(const
  // decltype(decl::conversion_function::body)&, ptype, ptype); void
  // emplace_glob(std::string&&,
  //                   const decltype(decl::conversion_function::body)&,
  //                   const decl::glob_conversion::condition_t&,
  //                   const decl::glob_conversion::extractor_t&);

  // private:
  //   container_type m_direct_store;
  //   std::vector<decl::glob_conversion> m_glob_store;
};

struct function_store : hashmap<callable_signature, const decl::function*> {
  function_store() = default;
  using hashmap::hashmap;
  using key_type   = hashmap::key_type;
  using value_type = hashmap::value_type;
  std::vector<value_type> get_by_name(const std::string&) const;
  void insert(decl::function*);
};

namespace selection {
struct if_ : visitable<if_, statement> {
  ast::keyword keyword;
  expr::expression& condition;
  decl::block* block;
  decl::block* else_;

  if_(const token&, expr::expression&, decl::block*, decl::block* = nullptr);
};

}; // namespace selection

namespace iteration {
struct iteration : public statement {
  ~iteration() override = default;
};

struct while_ : visitable<while_, statement> {
  ast::keyword keyword;
  expr::expression& condition;
  decl::block* body;
  while_(const token&, expr::expression&, decl::block*);
};

DERIVE_OK(statement, while_);
static_assert(!std::is_abstract_v<while_>);

struct for_ : visitable<for_, statement> {
  ast::keyword keyword;
  decl::variable* start;
  expr::expression* condition;
  expr::expression* step;
  decl::block* body;
  for_(const token& t,
       decl::variable* start_,
       expr::expression* condition_,
       expr::expression* step_,
       decl::block* block);
};

}; // namespace iteration

namespace jump {
struct goto_ : visitable<goto_, statement> {
  identifier term;
  goto_(const token&);
};

struct break_ : visitable<break_, statement> {
  ast::keyword keyword;
  explicit break_(const token& token);
};

struct continue_ : visitable<continue_, statement> {
  ast::keyword keyword;
  explicit continue_(const token& token);
};

struct return_ : visitable<return_, statement> {
  ast::keyword keyword;
  expr::expression* expr;
  return_(ast::keyword&& k, expr::expression* expr_);
};

} // namespace jump

struct translation_unit : visitable<translation_unit, scope> {
  translation_unit() = default;

  translation_unit(global_declarations decl)
      : stmts(std::move(decl)) {}

  [[nodiscard]] decl::function::definition* active_frame() noexcept { return m_stackframe.top(); }

  [[nodiscard]] const decl::function::definition* active_frame() const noexcept {
    return m_stackframe.top();
  }

  [[nodiscard]] scope* active_scope() noexcept;
  [[nodiscard]] const scope* active_scope() const noexcept;
  void clear() noexcept;

  // Frames
  void create_frame(decl::function::definition*);
  size_t pop_frame();

  template <typename T>
  bool is_declared(const ast::identifier&) const noexcept;
  template <typename T>
  bool is_declarable(const ast::identifier&) const noexcept;
  const decl::label* get_label(const ast::identifier&) const;
  std::vector<const decl::function*> user_defined_functions(const ast::identifier&) const;
  const variable_store::value_type& get_variable(const ast::identifier&) const override;
  void declare_function(ast::decl::function*, bool = false);
  assembly::operand* declare_variable(ast::decl::variable*) override;

  bool is_entry_point_defined() const noexcept;
  const decl::function* get_entry_point();
  void link_entry_point(ast::decl::function*);
  [[nodiscard]] bool is_global_scope() const noexcept;
  bool in_main() const noexcept;
  void set_context(ir::compilation_unit*);

  siblings<declaration*> stmts;

  friend ir::compilation_unit;

private:
  function_store m_functions;
  stack<decl::function::definition*> m_stackframe;
  ir::compilation_unit* m_context{};
};

using namespace_ = translation_unit;

#define TRACE_VISITOR(OBJ)                                \
  REGISTER_TRACE("{} visited {}",                         \
                 cpptrace::demangle(typeid(this).name()), \
                 cpptrace::demangle(typeid(OBJ).name()))

#define TERM_TYPES \
  ast::literal, ast::identifier, ast::keyword, ast::operator_, ast::storage, ast::linkage, ast::type

#define GLOBAL_TYPES ast::decl::function, ast::decl::variable

#define STATEMENT_TYPES                                                                \
  ast::decl::label, ast::iteration::for_, ast::iteration::while_, ast::selection::if_, \
      ast::jump::break_, ast::jump::continue_, ast::jump::goto_, ast::jump::return_,   \
      ast::decl::function::definition, ast::decl::block

#define CHILDREN_TYPES ast::decl::specifiers, ast::expr::arguments

#define EXPRESSION_TYPES                                                                         \
  ast::expr::binary_operator, ast::expr::unary_operator, ast::expr::call, ast::expr::identifier, \
      ast::expr::literal, ast::expr::implicit_type_conversion

#define NODE_TYPES STATEMENT_TYPES, EXPRESSION_TYPES, TERM_TYPES, CHILDREN_TYPES, GLOBAL_TYPES

struct ast_visitor : public visitor<NODE_TYPES> {
  void visit(ast ::expr ::identifier&) override;
  void visit(ast::decl::specifiers&) override;
  void visit(ast::decl::function::definition&) override;
  void visit(ast::literal&) override;
  void visit(ast::keyword&) override;
  void visit(ast::storage&) override;
  void visit(ast::type&) override;
  void visit(ast::linkage&) override;
  void visit(ast::expr::arguments&) override;
  void visit(ast::identifier&) override;
  void visit(ast ::expr ::unary_operator& c) override;
  void visit(ast ::expr ::literal& c) override;
  void visit(ast ::expr ::binary_operator& c) override;
  void visit(ast ::expr ::call& c) override;
  void visit(ast ::expr ::implicit_type_conversion& c) override;
  void visit(ast ::decl ::variable& c) override;
  void visit(ast ::decl ::function& c) override;
  void visit(ast ::decl ::label& c) override;
  void visit(ast::decl::block&) override;
  void visit(ast ::iteration ::while_& c) override;
  void visit(ast ::iteration ::for_& c) override;
  void visit(ast ::selection ::if_& c) override;
  void visit(ast ::jump ::goto_& c) override;
  void visit(ast ::jump ::return_& c) override;
  void visit(ast::jump::continue_& c) override;
  void visit(ast::operator_&) override;
  void visit(ast::jump::break_& c) override;
}; // namespace cmm::ast

struct const_ast_visitor : const_visitor<NODE_TYPES> {
  void visit(const ast ::expr ::identifier&) override;
  void visit(const ast::decl::specifiers&) override;
  void visit(const ast::decl::function::definition&) override;
  void visit(const ast::decl::block&) override;
  void visit(const ast::literal&) override;
  void visit(const ast::keyword&) override;
  void visit(const ast::storage&) override;
  void visit(const ast::type&) override;
  void visit(const ast::operator_&) override;
  void visit(const ast::linkage&) override;
  void visit(const ast::identifier&) override;
  void visit(const ast::expr::arguments&) override;
  void visit(const ast ::expr ::literal& c) override;
  void visit(const ast ::expr ::unary_operator& c) override;
  void visit(const ast ::expr ::binary_operator& c) override;
  void visit(const ast ::expr ::call& c) override;
  void visit(const ast ::expr ::implicit_type_conversion& c) override;
  void visit(const ast ::decl ::variable& c) override;
  void visit(const ast ::decl ::function& c) override;
  void visit(const ast ::decl ::label& c) override;
  void visit(const ast ::iteration ::while_& c) override;
  void visit(const ast ::iteration ::for_& c) override;
  void visit(const ast ::selection ::if_& c) override;
  void visit(const ast ::jump ::goto_& c) override;
  void visit(const ast ::jump ::return_& c) override;
  void visit(const ast::jump::continue_& c) override;
  void visit(const ast::jump::break_& c) override;
};

struct generic_node_visitor {
  virtual ~generic_node_visitor() = default;

  // entry point for visiting any node
  void visit(const node& n) { visit_impl(n); }

protected:
  // Override this to implement your check logic on any node
  virtual void check(const node& n) = 0;

private:
  void visit_impl(const node& n) {
    check(n); // call user-defined check

    // recursively visit children if this is a composite
    if (const auto* c = dynamic_cast<const composite*>(&n)) {
      for (const node* child : c->m_data) {
        if (child != nullptr) {
          visit_impl(*child);
        }
      }
    }
  }
};

struct check_visitor : generic_node_visitor {
  void check(const node& n) override {
    if (const auto* compo = dynamic_cast<const composite*>(&n)) {
      if (compo->m_data.empty()) {
        res.push_back(
            std::format("Object {} has no children", cpptrace::demangle(typeid(compo).name())));
      }
    }
  }

private:
  std::vector<std::string> res;
};
} // namespace cmm::ast

#include "ast.inl"
