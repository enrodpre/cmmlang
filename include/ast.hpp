#pragma once

#include "common.hpp"
#include "lang.hpp"
#include "macros.hpp"
#include "term.h"
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
class operand;
}

namespace cmm::ir {
class compilation_unit;
}
namespace cmm::ast {

struct statement : public virtual composite {
  using composite::composite;
  MOVABLE_CLS(statement);
  NOT_COPYABLE_CLS(statement);
};

struct empty_statement_t : visitable<statement, empty_statement_t> {
  empty_statement_t() = default;
  std::string string() const override { return "empty_statement"; }
};

inline empty_statement_t _empty_statement{};
inline empty_statement_t* empty_statement = &_empty_statement;

namespace expr {
  struct expression;
  struct identifier;
  struct unary_operator;
  struct call;
  struct binary_operator;
  struct string_literal;
  struct char_literal;
  struct sint_literal;
  struct uint_literal;
  struct false_literal;
  struct true_literal;
  struct float_literal;
}; // namespace expr

template <typename T>
concept has_type = requires(T t) {
  { t.type() } -> std::same_as<cmm::location>;
};

template <has_type T>
constexpr static auto EXTRACT_TYPE = [](const T& t) { return t.type(); };

using mangled_key                  = std::string;

struct anonymous_declaration : public statement {
  using statement::statement;
};

struct declaration : public anonymous_declaration {
  using anonymous_declaration::anonymous_declaration;
  identifier ident;
  declaration(const decltype(ident)& t)
      : ident(t) {}
  std::string string() const final { return ident.string(); }
};

namespace decl {
  class function;
  class variable;
  class label;
}; // namespace decl

struct variable_store : public hashmap<mangled_key, decl::variable*> {
  variable_store() = default;
  void put(decl::variable*);
};

// constexpr auto static CAST_TO_NODE = [](auto&& elem) { return dynamic_cast<node*>(elem); };

template <typename T>
struct siblings : public vector<T>, public visitable<composite, siblings<T>> {
  siblings() = default;
  siblings(std::initializer_list<T> init)
      : vector<T>(init) {}
  siblings(std::vector<T>&& s) noexcept
      : vector<T>(std::move(s)) {}
  siblings(const std::vector<T>& s) noexcept
      : vector<T>(std::move(s)) {}

  [[nodiscard]] cmm::location location() const final {
    return std::ranges::fold_left(*this | std::views::transform([](const T& n) {
      if constexpr (std::is_pointer_v<std::remove_cvref_t<T>>) {
        return n->location();
      } else {
        return n.location();
      }
    }),
                                  location(),
                                  std::plus<cmm::location>{});
  }

  // void accept(visitor<>& v) override {
  //   for (auto&& elem : *this) {
  //     if constexpr (std::is_pointer_v<T>) {
  //       elem->accept(v);
  //     } else {
  //       elem.accept(v);
  //     }
  //   }
  // }
  // void accept(visitor<>& v) const override {
  //   for (auto&& elem : *this) {
  //     if constexpr (std::is_pointer_v<T>) {
  //       elem->accept(v);
  //     } else {
  //       elem.accept(v);
  //     }
  //   }
  // }
  [[nodiscard]] std::string string() const override {
    return cpptrace::demangle(typeid(this).name());
  }
};

DERIVE_OK(composite, statement);

using global_declarations = siblings<declaration*>;
using statements          = siblings<statement*>;

struct scope : public declaration {
  using declaration::declaration;
  [[nodiscard]] virtual bool is_declared(const ast::identifier&) const;
  virtual decl::variable* get_variable(const ast::identifier&);
  [[nodiscard]] virtual const decl::variable* get_variable(const ast::identifier&) const;
  variable_store variables;

  friend class translation_unit;

protected:
  virtual void declare_variable(ast::decl::variable*);
};

template <typename T>
struct executable_scope : scope, siblings<T> {};

using label_store = std::unordered_map<std::string, const decl::label*>;

namespace decl {
  struct block : visitable<scope, block> {
    block(const statements& s)
        : stmts(s) {
      add(s.data());
    }
    void declare_label(const decl::label*);
    // AST_SIBLINGS()
    statements stmts;
    label_store labels;
  };

  struct specifiers : visitable<composite, specifiers> {
    ast::type type;
    ast::linkage linkage;
    ast::storage storage;
    specifiers(const ast::type&, const ast::linkage&, const ast::storage&);
    specifiers(cr_type t, const decltype(linkage)& l = {}, const decltype(storage)& s = {})
        : type(t),
          linkage(l),
          storage(s) {}
    cmm::location location() const override {
      return type.location() + linkage.location() + storage.location();
    }
    AST_COMPOSITE(&type, &linkage, &storage)
  };
  struct rank : visitable<composite, rank> {
    ast::operator_ open;
    expr::expression* number;
    ast::operator_ close;

    rank(const token&, const token&);
    rank(const token&, decltype(number), const token&);
    AST_COMPOSITE(&open, &linkage, &storage);
  };
  struct symbol {

    assembly::operand* address{};
  };
  struct label : public visitable<declaration, label>, public symbol {
    label(const token&);
    // FORMAT_DECL_IMPL();
    cmm::location location() const override;
    // AST_COMPOSITE(ident)
  };

  struct variable : visitable<declaration, variable>, public symbol {
    specifiers specs;
    decl::rank* rank;
    expr::expression* init;

    variable(const specifiers&, decltype(rank), identifier&&, decltype(init));
    variable(cr_type, decltype(ident)&&, decltype(init));
    variable(cr_type, std::string);
    cmm::location location() const override;
    // std ::string string() const override { return ident.value(); }
    std::string repr() const override { return std::format("variable_{}", string()); }
  };

  struct signature;
  struct function : visitable<declaration, function>, public symbol {
    using parameter         = variable*;
    using loaded_parameters = std::vector<parameter>;

    struct definition;
    struct parameters : public visitable<siblings<parameter>, parameters> {
      using vector_t = vector<parameter>;
      parameters(siblings<parameter>&& p)
          : visitable(std::move(p)) {}
      void load_arguments(const siblings<expr::expression*>&);
      [[nodiscard]] std::vector<ptr_type> types() const {
        return vector<parameter>::data() |
               std::views::transform(
                   [](const parameter& param) -> ptr_type { return &param->specs.type.value(); }) |
               std::ranges::to<std::vector>();
      }
      std ::string string() const override { return cpptrace ::demangle(typeid(this).name()); }
    };

    specifiers specs;
    decl::function::parameters params;
    definition* body;

    function(const decltype(specs)&, decltype(ident)&&, const decltype(params)&, decltype(body));
    function(const operator_& name, ptr_type ret, const parameters& params, decltype(body) b)
        : specs(*ret),
          visitable(name),
          params(params),
          body(b) {}
    // FORMAT_DECL_IMPL();
    cmm::location location() const override;
    signature sig() const;
    // AST_COMPOSITE(ident, specs, params, body)
  };

  struct function::definition : visitable<block, definition> {
    definition(const siblings<statement*>& s)
        : visitable(s) {}
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
    [[nodiscard]] decl::variable* get_variable(const identifier& ident) override;
    [[nodiscard]] const decl::variable* get_variable(const identifier& ident) const override;
    assembly::operand* declare_parameter(ast::decl::variable*, assembly::operand*);
    size_t destroy_scope() noexcept;
    void clear() noexcept;
    label_store labels;
    stack<block*> local_scopes;
  };
  static_assert(std::is_base_of_v<block, function::definition>);
  struct signature : public formattable {
    identifier name;
    std::vector<ptr_type> types;
    signature(std::string n, const std::vector<ptr_type>& t)
        : name(std::move(n)),
          types(t) {}
    signature(const identifier& id, const std::vector<ptr_type>& t)
        : name(id),
          types(t) {}

    bool operator==(const signature& other) const {
      return name.value() == other.name.value() &&
             std::ranges::all_of(std::views::zip(types, other.types), [](const auto& type_pair) {
               const auto& [t, other_t] = type_pair;
               return t == other_t;
             });
    }
    [[nodiscard]] std::string format() const override {
      return mangled_name::function(name.value(), types);
    }
    operator std::string() const { return format(); }
  };

  // static_assert(std::formattable<function, char>);
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
    [[nodiscard]] virtual bool is_convertible(cr_type) const noexcept = 0;
    [[nodiscard]] virtual cr_type to(cr_type) const noexcept          = 0;
    [[nodiscard]] virtual assembly::operand* operator()(assembly::operand*) const noexcept;
    [[nodiscard]] bool is_implicit() const { return type == conversion_type_t::IMPLICIT; };
    [[nodiscard]] bool is_explicit() const { return type == conversion_type_t::EXPLICIT; };
  };

}; // namespace decl

class conversion_store {
public:
  // using key_type       = mangled_key;
  // using value_type     = std::unordered_map<key_type, decl::direct_conversion>;
  // using container_type = std::unordered_map<key_type, value_type>;

  // conversion_store() = default;
  // [[nodiscard]] bool is_convertible(cr_type, cr_type) const;
  // [[nodiscard]] std::vector<ptr_type> get_convertible_types(cr_type) const;
  // [[nodiscard]] std::vector<const decl::conversion_function*> get_conversions(cr_type) const;
  // void emplace_direct(const decltype(decl::conversion_function::body)&, cr_type, cr_type);
  // void emplace_glob(std::string&&,
  //                   const decltype(decl::conversion_function::body)&,
  //                   const decl::glob_conversion::condition_t&,
  //                   const decl::glob_conversion::extractor_t&);

  // private:
  //   container_type m_direct_store;
  //   std::vector<decl::glob_conversion> m_glob_store;
};

struct function_store : hashmap<decl::signature, decl::function*> {
  function_store() = default;
  using hashmap::hashmap;
  using key_type   = hashmap::key_type;
  using value_type = hashmap::value_type;
  std::vector<value_type> get_by_name(cstring) const;
  void insert(decl::function*);
};

namespace selection {
  struct if_ : visitable<statement, if_> {
    ast::keyword keyword;
    expr::expression& condition;
    decl::block* block;
    decl::block* else_;

    if_(const token&, expr::expression&, decl::block*, decl::block* = nullptr);
    // FORMAT_DECL_IMPL();
    cmm::location location() const override;
    AST_COMPOSITE(keyword, condition, block, else_)
  };

}; // namespace selection

namespace iteration {
  struct iteration : public statement {
    ~iteration() override = default;
  };

  struct while_ : visitable<statement, while_> {
    ast::keyword keyword;
    expr::expression& condition;
    decl::block* body;
    while_(const token&, expr::expression&, decl::block*);
    // FORMAT_DECL_IMPL();
    cmm::location location() const override;
    AST_COMPOSITE(keyword, condition, body)
  };

  DERIVE_OK(statement, while_);
  static_assert(!std::is_abstract_v<while_>);

  struct for_ : visitable<statement, for_> {
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
    // FORMAT_DECL_IMPL();
    cmm::location location() const override;
    AST_COMPOSITE(keyword, start, condition, step, body)
  };

}; // namespace iteration

namespace jump {
  struct goto_ : visitable<statement, goto_> {
    identifier term;
    goto_(const token&);
    AST_COMPOSITE(term)
    cmm::location location() const override;
  };
  struct break_ : visitable<statement, break_> {
    ast::keyword keyword;
    explicit break_(const token& token);
    AST_COMPOSITE(keyword)
    cmm::location location() const override;
  };

  struct continue_ : visitable<statement, continue_> {
    ast::keyword keyword;
    explicit continue_(const token& token);
    AST_COMPOSITE(keyword)
    cmm::location location() const override;
  };

  struct return_ : visitable<statement, return_> {
    ast::keyword keyword;
    expr::expression* expr;
    return_(const ast::keyword& k, expr::expression* expr_);
    AST_COMPOSITE(keyword, expr)
    cmm::location location() const override;
  };

} // namespace jump

struct translation_unit : visitable<scope, translation_unit> {
  translation_unit() = default;
  translation_unit(const global_declarations& decl)
      : stmts(decl) {}

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
  decl::function* get_function(const decl::signature&);
  const decl::function* get_function(const decl::signature&) const;
  decl::variable* get_variable(const ast::identifier&) override;
  const decl::variable* get_variable(const ast::identifier&) const override;
  void declare_function(ast::decl::function*, bool = false);
  void declare_variable(ast::decl::variable*) override;
  // std::vector<ptr_type> get_conversions(cr_type t) {
  //   return m_conversions.get_convertible_types(t);
  // }

  bool is_entry_point_defined() const noexcept;
  const decl::function* get_entry_point();
  void link_entry_point(ast::decl::function*);
  [[nodiscard]] bool is_global_scope() const noexcept;
  bool in_main() const noexcept;
  void set_context(ir::compilation_unit*);

  // std::string string() const override { return "Translation unit:"; }

  siblings<declaration*> stmts;

private:
  function_store m_functions;
  stack<decl::function::definition*> m_stackframe;
  ir::compilation_unit* m_context{};

  std::optional<decl::function*> progressive_prefix_match(
      const std::vector<ptr_type>&,
      const std::vector<decl::function*>&) const;
};
using namespace_ = translation_unit;

#define TRACE_VISITOR(OBJ) \
  REGISTER_TRACE("{} visited {}", \
                 cpptrace::demangle(typeid(this).name()), \
                 cpptrace::demangle(typeid(OBJ).name()))

#define TERM_TYPES \
  ast::literal, ast::identifier, ast::keyword, ast::operator_, ast::storage, ast::linkage, ast::type

#define GLOBAL_TYPES ast::decl::function, ast::decl::variable

#define STATEMENT_TYPES \
  ast::decl::label, ast::iteration::for_, ast::iteration::while_, ast::selection::if_, \
      ast::jump::break_, ast::jump::continue_, ast::jump::goto_, ast::jump::return_

#define CHILDREN_TYPES ast::decl::function::parameters, ast::decl::specifiers

#define LITERAL_TYPES \
  ast::expr::string_literal, ast::expr::sint_literal, ast::expr::uint_literal, \
      ast::expr::char_literal, ast::expr::float_literal, ast::expr::false_literal, \
      ast::expr::true_literal
#define EXPRESSION_TYPES \
  ast::expr::binary_operator, ast::expr::unary_operator, ast::expr::call, ast::expr::identifier

#define NODE_TYPES STATEMENT_TYPES, EXPRESSION_TYPES, TERM_TYPES, CHILDREN_TYPES, GLOBAL_TYPES

struct ast_visitor : visitor<NODE_TYPES> {
  void visit(ast ::expr ::identifier&) override;
  void visit(ast::decl::specifiers&) override;
  void visit(ast::decl::function::parameters&) override;
  void visit(ast::literal&) override;
  void visit(ast::keyword&) override;
  void visit(ast::identifier&) override;
  // void visit(ast::expr::arguments&) override;
  void visit(ast ::expr ::unary_operator& c) override;
  void visit(ast ::expr ::binary_operator& c) override;
  void visit(ast ::expr ::call& c) override;
  void visit(ast ::decl ::variable& c) override;
  void visit(ast ::decl ::function& c) override;
  void visit(ast ::decl ::label& c) override;
  void visit(ast ::iteration ::while_& c) override;
  void visit(ast ::iteration ::for_& c) override;
  void visit(ast ::selection ::if_& c) override;
  void visit(ast ::jump ::goto_& c) override;
  void visit(ast ::jump ::return_& c) override;
  void visit(ast::jump::continue_& c) override;
  void visit(ast::jump::break_& c) override;
}; // namespace cmm::ast
struct const_ast_visitor : const_visitor<NODE_TYPES> {
  void visit(const ast ::expr ::identifier&) override;
  void visit(const ast::decl::specifiers&) override;
  void visit(const ast::decl::function::parameters&) override;
  void visit(const ast::literal&) override;
  void visit(const ast::keyword&) override;
  void visit(const ast::operator_&) override;
  void visit(const ast::identifier&) override;
  // void visit(const ast::expr::arguments&) override;
  void visit(const ast ::expr ::unary_operator& c) override;
  void visit(const ast ::expr ::binary_operator& c) override;
  void visit(const ast ::expr ::call& c) override;
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
} // namespace cmm::ast
#include "ast.inl"
