#pragma once

#include "common.hpp"
#include "lang.hpp"
#include "macros.hpp"
#include "memory.hpp"
#include "revisited/visitor.h"
#include "stl.hpp"
#include "token.hpp"
#include "traits.hpp"
#include "types.hpp"
#include <algorithm>
#include <bits/ranges_algo.h>
#include <initializer_list>

#include <magic_enum/magic_enum.hpp>
#include <magic_enum/magic_enum_format.hpp>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>

namespace cmm {
namespace assembly {
struct element;
struct reg;
struct operand;
struct memory;
struct stack_memory;
} // namespace assembly

namespace ir {
struct compilation_unit;
}

struct callable;
namespace ast {

namespace expr {
struct expression;
struct unary_operator;
struct call;
struct binary_operator;
} // namespace expr

using namespace revisited;

template <typename... Visitable>
using visitor = revisited::Visitor<std::add_lvalue_reference_t<Visitable>...>;
template <typename... Visitable>
using const_visitor = visitor<std::add_const_t<Visitable>...>;

struct translation_unit;

struct node : public revisited::Visitable<node>, displayable {
  ~node() override = default;
  node(std::optional<cmm::location> l = {})
      : m_location(std::move(l)) {}
  node(const token& t)
      : node(t.location()) {}
  MOVABLE_CLS(node);
  COPYABLE_CLS(node);
  template <typename T = node>
  T* get_parent() {
    return dynamic_cast<T*>(m_parent);
  }
  template <typename T = node>
  const T* get_parent() const {
    return dynamic_cast<const T*>(m_parent);
  }
  virtual void set_parent(node* parent_) { m_parent = parent_; }
  virtual void set_parent(node* parent_) const { m_parent = parent_; }
  virtual std::optional<cmm::location> location() const { return m_location; };
  operator node*() { return static_cast<node*>(this); }
  std::string repr() const override { return cpptrace::demangle(typeid(*this).name()); }
  std::string string() const override { return repr(); }
  void add_child(node* t_node) {
    if (t_node != nullptr) {
      t_node->set_parent(this);
      m_location = m_location + t_node->location();
      m_children.push_back(t_node);
    }
  }
  virtual std::vector<node*> children() = 0;
  std::vector<const node*> children() const { return m_children; }
  translation_unit* get_root();
  const translation_unit* get_root() const;
  void initialize(ast::translation_unit* t_tu) {
    m_root = t_tu;
    for (node* child : children()) {
      add_child(child);
    }
  }

private:
  mutable node* m_parent   = nullptr;
  translation_unit* m_root = nullptr;
  std::optional<cmm::location> m_location;
  std::vector<const node*> m_children;
};

template <typename T>
concept ast_node = std::is_base_of_v<ast::node, T>;

template <typename T>
concept pointer_ast_node = std::is_assignable_v<T, ast::node*>;

struct identifier;
struct literal;

#define children_impl(CHILDREN)                                                   \
  std::vector<node*> children() override { return std::vector<node*>{CHILDREN}; }

#define to_node(OBJ) static_cast<node*>(OBJ)
template <typename T>
struct term : derived_visitable<term<T>, node> {
  std::vector<node*> children() final { return {}; }

  term() = default;
  COPYABLE_CLS(term);
  term(T t_t)
      : m_value(t_t) {}
  term(const token& t_token, T t_t)
      : derived_visitable<term<T>, node>(t_token.location()),
        m_value(t_t) {}
  term(std::optional<cmm::location> t_loc, T t_t)
      : derived_visitable<term<T>, node>(t_loc),
        m_value(t_t) {}
  operator const T&() const { return value(); }
  const T& value() const { return m_value; }
  [[nodiscard]] std::string string() const override { return std::format("{}", m_value); }

private:
  T m_value{};
};

struct keyword : public derived_visitable<keyword, term<keyword_t>> {
  keyword(const token& t_token, keyword_t t_keyword)
      : derived_visitable(t_token.location(), t_keyword) {}
  using derived_visitable::derived_visitable;
};

static_assert(std::is_assignable_v<keyword, node>);

struct literal : public derived_visitable<literal, term<std::string_view>> {
  literal(cmm::location l, std::string_view s)
      : derived_visitable(std::move(l), s) {}
};

struct operator_ : public derived_visitable<operator_, term<operator_t>> {
  operator_(const cmm::token& t)
      : derived_visitable(t.location(), token_data(t.type).cast<operator_t>()) {}

  operator_(const token& t, operator_t op)
      : derived_visitable(t.location(), op) {}
};

struct storage_spec : derived_visitable<storage_spec, term<storage_t>> {
  storage_spec() = default;
  storage_spec(const token& t_token, storage_t t_storage)
      : derived_visitable(t_token, t_storage) {}
};

struct linkage_spec : derived_visitable<linkage_spec, term<linkage_t>> {
  linkage_spec() = default;
  linkage_spec(const token& t_token, linkage_t t_linkage)
      : derived_visitable(t_token, t_linkage) {}
};
struct type_spec : derived_visitable<type_spec, term<type_id>> {
  type_spec(type_id t_type)
      : derived_visitable(t_type) {}
  type_spec(const token& t_token, type_id t_type)
      : derived_visitable(t_token, t_type) {}
};

struct identifier : derived_visitable<identifier, term<std::string>> {
  identifier(std::optional<cmm::location> t_loc, std::string t_ident)
      : derived_visitable(t_loc, t_ident) {}
  identifier(std::string mangled)
      : identifier({}, std::move(mangled)) {}
  identifier(const token& t_token)
      : identifier(t_token.location(), std::string(t_token.value)) {}
  operator std::string_view() const { return value(); }
};

struct statement : derived_visitable<statement, node> {
  using derived_visitable<statement, node>::derived_visitable;
  statement(statement&&) noexcept = default;

  statement& operator=(statement&& other) noexcept {
    node::operator=(std::move(other));
    return *this;
  }

  COPYABLE_CLS(statement);
};

struct empty_statement_t : derived_visitable<empty_statement_t, statement> {
  empty_statement_t() = default;

  // std::string string() const override { return "empty_statement"; }
};

using mangled_key = std::string;

struct anonymous_declaration : derived_visitable<anonymous_declaration, statement> {
  anonymous_declaration() = default;
  using derived_visitable::derived_visitable;
};

struct declaration : derived_visitable<declaration, anonymous_declaration> {
  using derived_visitable::anonymous_declaration;
  ast::identifier ident;

  declaration() = delete;
  declaration(decltype(ident) t)
      : ident(std::move(t)) {}
  declaration(const token& t_token)
      : ident(t_token) {}
};

namespace decl {
struct function;
struct variable;
struct label;
}; // namespace decl

struct variable_store
    : public hashmap<mangled_key, std::pair<const decl::variable*, assembly::operand*>> {
  using key_type     = mangled_key;
  using value_type   = std::pair<const decl::variable*, assembly::operand*>;
  using symbol_type  = std::tuple_element<0, value_type>;
  using address_type = std::tuple_element_t<1, value_type>;

  variable_store()   = default;
  void insert(const decl::variable*, assembly::operand*);
};

// dynamic_cast<node*>(elem); };

template <typename T>
struct siblings : public derived_visitable<siblings<T>, node> {
  using value_type     = T;
  using container_type = std::vector<T, cmm::memory::allocator<T>>;
  using allocator_type = cmm::memory::allocator<T>;

  siblings()           = default;
  siblings(std::initializer_list<T> init)
      : m_data(init) {}

  siblings(siblings<T>&&)                 = default;
  siblings& operator=(siblings<T>&&)      = default;
  siblings(const siblings<T>&)            = default;
  siblings& operator=(const siblings<T>&) = default;
  siblings(std::vector<T>&& s) noexcept
      : m_data(std::move(s)) {}
  siblings(const std::vector<T>& s) noexcept
      : m_data(s) {}

  container_type::iterator begin() { return m_data.begin(); }
  container_type::iterator end() { return m_data.end(); }
  container_type::const_iterator begin() const { return m_data.begin(); }
  container_type::const_iterator end() const { return m_data.begin(); }
  [[nodiscard]] size_t size() const noexcept { return m_data.size(); }
  void push_back(const T& t_t) { return m_data.push_back(t_t); }
  void push_back(T&& t_t) { return m_data.push_back(std::move(t_t)); }
  const container_type& data() const { return m_data; }

  operator container_type() { return m_data; }
  operator const container_type&() const { return m_data; }
  std::vector<node*> children() override {
    if constexpr (std::is_pointer_v<std::remove_cvref_t<T>>) {
      return std::vector<node*>{
          m_data | std ::views ::transform([](auto* s) { return dynamic_cast<node*>(s); }) |
          std::ranges::to<std::vector>()};
    } else {
      return std::vector<node*>{
          m_data | std ::views ::transform([](auto& s) { return dynamic_cast<node*>(&s); }) |
          std::ranges::to<std::vector>()};
    }
  };

private:
  std::vector<T, cmm::memory::allocator<T>> m_data;
};

namespace expr {
using arguments = siblings<expr::expression*>;
}

DERIVE_OK(node, statement);

using global_declarations = siblings<declaration*>;
using statements          = siblings<statement*>;

struct scope : derived_visitable<scope, anonymous_declaration> {
  using derived_visitable::derived_visitable;
  [[nodiscard]] virtual bool is_declared(const ast::identifier&) const noexcept;
  [[nodiscard]] virtual const variable_store::value_type& get_variable(
      const ast::identifier&) const;
  [[nodiscard]] const ast::decl::variable* get_variable_declaration(const ast::identifier&) const;
  [[nodiscard]] variable_store::address_type get_variable_address(const ast::identifier&) const;

  variable_store variables;

  friend struct translation_unit;

  virtual variable_store::address_type declare_variable(ast::decl::variable*);
};

template <typename T>
struct executable_scope : scope, siblings<T> {};

using label_store = std::unordered_map<std::string, const decl::label*>;

namespace decl {
struct block : derived_visitable<block, scope> {
  block(statements s)
      : stmts(std::move(s)) {}

  statements stmts;
  children_impl(stmts | TRANSFORM([](const auto& s) { return dynamic_cast<node*>(s); }) | TO_VEC);
  // std::string string() const override { return "Block"; }
};

struct specifiers : derived_visitable<specifiers, node> {
  type_spec type;
  linkage_spec linkage;
  storage_spec storage;
  specifiers(type_spec&&, linkage_spec&&, storage_spec&&);

  specifiers(types::type_id t, decltype(linkage) l = {}, decltype(storage) s = {})
      : type(t),
        linkage(std::move(l)),
        storage(std::move(s)) {}
  std::vector<node*> children() override {
    return {to_node(&type), dynamic_cast<node*>(&linkage), dynamic_cast<node*>(&storage)};
  };
  // std::string string() const override { return std::format("{} {} {}", type, linkage, storage); }
};

struct rank : derived_visitable<rank, node> {
  ast::operator_ open;
  expr::expression* number;
  ast::operator_ close;

  rank(const token&, const token&);
  rank(const token&, decltype(number), const token&);
  std ::vector<node*> children() override;
  // std::string string() const override;
};

struct label : derived_visitable<label, declaration> {
  label(const token&);
  std::vector<node*> children() override { return {&ident}; }
  // std::string string() const override { return std::format("label {}", ident); }
};

struct variable : derived_visitable<variable, declaration> {
  specifiers specs;
  decl::rank* rank;
  expr::expression* init;

  variable(specifiers&&, decltype(rank), identifier, decltype(init));
  variable(types::type_id, decltype(ident), decltype(init));

  // std::string repr() const override { return std::format("VarDecl", string()); }
  std::vector<node*> children() override;
  // std::string string() const override;
};

struct function : derived_visitable<function, declaration>, public callable {
  struct definition;

  specifiers specs;
  siblings<variable> params;
  definition* body;

  function(decltype(specs)&&, const token&, decltype(params)&&, decltype(body));

  bool is_user_defined() const override { return true; }
  ast::identifier identifier() const override { return ident; }
  cmm::parameters parameters_impl() const override;
  bool is_defined() const override { return body == nullptr; }
  type_id return_type() const override { return specs.type.value(); }
  std::vector<node*> children() override;
  // std::string repr() const override;
  std::string string() const override;
};

struct function::definition : derived_visitable<definition, block> {
  definition(const siblings<statement*>& s)
      : derived_visitable(s) {
    local_scopes.push(this);
  }

  struct {
    [[nodiscard]] size_t size() const { return stack_size; };
    void push() { stack_size++; };
    void pop(size_t times = 1) { stack_size -= times; }

  private:
    size_t stack_size = 0;
  } local_stack;

  block* active_scope() { return local_scopes.top(); }
  const block* active_scope() const { return local_scopes.top(); }
  void create_scope(block&) noexcept;
  [[nodiscard]] const variable_store::value_type& get_variable(
      const ast::identifier&) const override;
  assembly::reg* declare_parameter(const ast::decl::variable&, assembly::reg*);
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
  [[nodiscard]] virtual bool is_convertible(types::type_id) const noexcept = 0;
  [[nodiscard]] virtual types::type_id to(types::type_id) const noexcept   = 0;
  [[nodiscard]] virtual assembly::element* operator()(assembly::element*) const noexcept;
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
  // [[nodiscard]] bool is_convertible(type, type) const;
  // [[nodiscard]] std::vector<type> get_convertible_types(type) const;
  // [[nodiscard]] std::vector<const decl::conversion_function*>
  // get_conversions(type) const; void emplace_direct(const
  // decltype(decl::conversion_function::body)&, type, type); void
  // emplace_glob(std::string&&,
  //                   const decltype(decl::conversion_function::body)&,
  //                   const decl::glob_conversion::condition_t&,
  //                   const decl::glob_conversion::extractor_t&);

  // private:
  //   container_type m_direct_store;
  //   std::vector<decl::glob_conversion> m_glob_store;
};
struct function_store : hashmap<std::string, const decl::function*> {
  function_store() = default;
  using key_type   = hashmap::key_type;
  using value_type = hashmap::value_type;
  std::vector<value_type> get_by_name(const std::string&) const;
  void insert(decl::function*);
};

namespace selection {
struct if_ : derived_visitable<if_, statement> {
  ast::keyword keyword;
  reference<expr::expression> condition;
  decl::block* block;
  decl::block* else_;

  if_(const token&, decltype(condition), decl::block*, decl::block* = nullptr);
  std::vector<node*> children() override;
};

}; // namespace selection

namespace iteration {

struct iteration : derived_visitable<iteration, statement> {
  virtual std::pair<std::string, std::string> labels() const = 0;
};

struct while_ : derived_visitable<while_, iteration> {
  ast::keyword keyword;
  reference<expr::expression> condition;
  decl::block* body;

  while_(const token&, expr::expression&, decl::block*);
  std::vector<node*> children() override;
  std::pair<std::string, std::string> labels() const override {
    return std::make_pair("cond_while", "exit_while");
  }
};

DERIVE_OK(statement, while_);
static_assert(!std::is_abstract_v<while_>);

struct for_ : derived_visitable<for_, iteration> {
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
  std::vector<node*> children() override;
  std::pair<std::string, std::string> labels() const override {
    return std::make_pair("cond_for", "exit_for");
  }
};

}; // namespace iteration

namespace jump {
struct goto_ : derived_visitable<goto_, statement> {
  identifier term;
  goto_(const token&);
  std::vector<node*> children() override;
};

struct break_ : derived_visitable<break_, statement> {
  ast::keyword keyword;
  explicit break_(const token& token);
  std::vector<node*> children() override;
};

struct continue_ : derived_visitable<continue_, statement> {
  ast::keyword keyword;
  explicit continue_(const token& token);
  std::vector<node*> children() override;
};

struct return_ : derived_visitable<return_, statement> {
  ast::keyword keyword;
  expr::expression* expr;
  return_(const token& t, expr::expression* expr_);
  std::vector<node*> children() override;
};

} // namespace jump

struct translation_unit : derived_visitable<translation_unit, scope> {
  translation_unit(global_declarations decl);
  NOT_COPYABLE_CLS(translation_unit);
  children_impl(stmts | TRANSFORM([](const auto& s) { return dynamic_cast<node*>(s); }) | TO_VEC);

  [[nodiscard]] decl::function::definition* active_frame() noexcept { return m_stackframe.top(); }
  [[nodiscard]] const decl::function::definition* active_frame() const noexcept {
    if (m_stackframe.empty()) {
      THROW(LABEL_IN_GLOBAL, nullptr);
    }
    return m_stackframe.top();
  }
  [[nodiscard]] scope* active_scope() noexcept;
  [[nodiscard]] const scope* active_scope() const noexcept;
  void clear() noexcept;

  // Frames
  void create_frame(decl::function::definition*);
  size_t pop_frame();

  bool is_declared(const ast::identifier&) const noexcept override;
  bool is_declarable(const ast::identifier&) const noexcept;
  const decl::label* get_label(const ast::identifier&) const;
  const variable_store::value_type& get_variable(const ast::identifier&) const override;
  variable_store::address_type declare_variable(ast::decl::variable*) override;
  void declare_function(ast::decl::function*);
  void define_function(ast::decl::function*);
  template <typename Id>
  const callable* get_callable(Id id, const expr::arguments&) const;

  [[nodiscard]] bool is_global_scope() const noexcept;
  bool in_main() const noexcept;

  std::vector<const ast::decl::function::definition*> stackframe();
  siblings<declaration*> stmts;
  function_store functions;
  stack<decl::function::definition*> m_stackframe;
  ir::compilation_unit* cunit;

  // GLOBAL DATA
  static std::unordered_map<operator_t, std::vector<builtin_callable>> operators;
  static const std::array<types::unary_matcher, types::unary_matchers.size()>* unary_matchers;
  static const std::array<types::modifier, types::modifiers.size()>* modifiers;
  static types::manager* types;
  static const std::array<const conversor*, 2> standard_conversions;
  static const magic_enum::containers::
      array<value_category_t, std::vector<std::pair<types::unary_matcher, binding_mode_t>>>
          binding_rules;
  static cmm::binding_mode_t bind_value(value_category_t, type_id);
  static ast::expr::expression* bind_expression(ast::expr::expression*, type_id);
  static bool is_bindable_to(value_category_t, types::type_id);

private:
  void load_arguments(const parameters&, const expr::arguments&);
  template <typename Id>
  std::vector<const callable*> get_candidates(Id) const;
  static const callable* resolve_overloads(std::vector<const callable*>, const expr::arguments&);
  [[nodiscard]] static bool match_arguments(const std::vector<types::type_id>&,
                                            const std::vector<types::type_id>&);
};

using namespace_ = translation_unit;

} // namespace ast
} // namespace cmm

#include "ast/tree.inl"
