#pragma once

#include "common.hpp"
#include "lang.hpp"
#include "token.hpp"
#include "traits.hpp"
#include "types.hpp"
#include "visitor.hpp"
#include <algorithm>
#include <bits/ranges_algo.h>
#include <cpptrace/utils.hpp>
#include <libassert/assert.hpp>
#include <magic_enum/magic_enum.hpp>
#include <magic_enum/magic_enum_format.hpp>
#include <string>
#include <type_traits>
#include <utility>

namespace cmm::assembly {
struct operand;
};

namespace cmm::ast {

template <typename T>
struct identifiable {
private:
  identifiable() = default;

public:
  virtual ~identifiable() = default;
  [[nodiscard]] virtual std::vector<const ast::node*> path() const;
  friend T;
};

namespace expr {
  struct expression;
  struct identifier;
  struct literal;
  struct call;
  struct arguments;
  struct unary_operator;
  struct binary_operator;
  struct string_literal;
  struct char_literal;
  struct sint_literal;
  struct uint_literal;
  struct false_literal;
  struct true_literal;
  struct float_literal;
}; // namespace expr
class node;

template <typename T>
concept has_type = requires(T t) {
  { t.type() } -> std::same_as<cmm::location>;
};

template <has_type T>
constexpr static auto EXTRACT_TYPE = [](const T& t) { return t.type(); };

using mangled_key                  = std::string;

#define FORMAT_DECL_IMPL() \
  std::string repr(size_t = 0) const override; \
  std::string format() const override;

template <typename Repr>
struct term : visitable<leaf> {
  term() = default;
  term(cmm::location l)
      : visitable(std::move(l)) {}
  term(const token& t)
      : visitable<leaf>(t) {}

  // [[nodiscard]] std::string repr(size_t) const override { return std::format("{}", value()); }
  // [[nodiscard]] std::string format() const override { return std::format("{}", value()); }
  operator const Repr&() const { return value(); }
  [[nodiscard]] virtual const Repr& value() const;
};

namespace terms {
  struct keyword : visitable<term<keyword_t>, keyword> {
    using visitable::visitable;
    [[nodiscard]] const keyword_t& value() const override { return m_value; }

  private:
    keyword_t m_value;
  };
  struct literal : visitable<term<std::string>, literal> {
    using visitable::visitable;
    literal(cmm::location, std::string);
    [[nodiscard]] const std::string& value() const override { return m_value; }

  private:
    std::string m_value;
  };
  struct operator_ : visitable<term<operator_t>, operator_> {
    [[nodiscard]] const operator_t& value() const override { return m_value; }

  private:
    operator_t m_value;
  };
  template <typename T>
  struct specifier : term<T> {
    using parent_t = term<T>;
    using parent_t::parent_t;
    specifier() = default;
    specifier(const token& x)
        : parent_t(x) {}
    friend T;
  };
  struct storage : visitable<specifier<storage_t>, storage> {
  private:
    storage_t m_value{};
  };
  struct linkage : visitable<specifier<linkage_t>, linkage> {

  private:
    linkage_t m_value{};
  };
  struct type : visitable<specifier<cr_type>, type> {
    type(cr_type t)
        : visitable(),
          m_value(t) {}

  private:
    cr_type m_value;
  };
  struct identifier : visitable<term<std::string>, identifier> {
    using visitable::visitable;
    identifier(std::string name)
        : m_value(std::move(name)) {}
    identifier(const operator_& op)
        : visitable(op.location()),
          m_value(std::format("operator{}", op.value())) {}

    [[nodiscard]] const std::string& value() const override { return m_value; }

  private:
    std::string m_value;
  };
} // namespace terms

struct declaration : public statement {
  declaration() = default;
  terms::identifier ident;
  declaration(decltype(ident)&& t)
      : ident(std::move(t)) {}
};

namespace decl {
  class variable;
};

using variable_store = hashmap<mangled_key, decl::variable>;
template <typename T>
class base_scope : public vector<T>, public composite {
public:
  using vector<T>::begin;
  using vector<T>::end;
  using vector<T>::cbegin;
  using vector<T>::cend;
  base_scope() = default;
  base_scope(std::vector<T>&& s) noexcept
      : vector<T>(std::move(s)) {}

  [[nodiscard]] cmm::location location() const override {
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
  // FORMAT_DECL_IMPL();

  variable_store variables;
};

DERIVE_OK(composite, statement);

class declaration;

namespace decl {
  class function;
  class variable;
}; // namespace decl

class stackframe;

struct global_scope : virtual visitable<base_scope<declaration*>, global_scope> {};
struct local_scope : virtual visitable<base_scope<statement*>, local_scope> {};
using function_store = hashmap<mangled_key, std::unique_ptr<decl::function>>;

namespace scopes {
  struct block : public declaration, virtual visitable<local_scope, block> {
    block(block&& v) noexcept
        : visitable(std::move(v)) {
      local_scope::set_parent(static_cast<local_scope*>(this));
    }
  };
  using function_scope = block;
}; // namespace scopes
// struct enum_ : visitable<statement, block> {};
// struct class_ : visitable<statement, block> {};

// template <typename T>
//   requires(Formattable<T>)
// struct lazy_format {
//   lazy_format(const T& t)
//       : m_obj(t),
//         m_func([](const T& t) { return t.format(); }) {}
//
//   cr_string value(const T& t) const {
//     if (!m_value.has_value()) {
//       m_value = m_func(t);
//     }
//     return m_value.value();
//   }
//
// private:
//   mutable std::optional<std::string> m_value;
//   std::reference_wrapper<const T> m_obj;
//   std::function<std::string(const T&)> m_func;
// };

namespace decl {
  struct specifiers : visitable<composite, specifiers> {
    terms::type type;
    terms::linkage linkage;
    terms::storage storage;
    specifiers(terms::type, terms::linkage, terms::storage);
    specifiers(cr_type t, decltype(linkage) l = {}, decltype(storage) s = {})
        : type(t),
          linkage(std::move(l)),
          storage(std::move(s)) {}
    // FORMAT_DECL_IMPL();
    cmm::location location() const override {
      return type.location() + linkage.location() + storage.location();
    }
  };
  struct label : public visitable<declaration, label> {
    label(const token&);
    // FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };

  struct variable : visitable<declaration, variable>, public identifiable<variable> {
    specifiers specs;
    expr::expression* init;

    variable(specifiers&&, terms::identifier&&, decltype(init));
    variable(cr_type, decltype(ident)&&, decltype(init));
    variable(cr_type, std::string);
    // FORMAT_DECL_IMPL();
    cmm::location location() const override;
    std::vector<const node*> path() const override;
  };

  struct function : visitable<declaration, function> {
    using parameter         = variable;
    using loaded_parameters = std::vector<parameter>;

    struct parameters : public visitable<base_scope<parameter>, parameters> {
      using parent_t = base_scope<parameter>;
      using vector_t = vector<parameter>;
      parameters()   = default;
      parameters(std::vector<parameter>&& v)
          : visitable(std::move(v)) {}
      parameters(const std::vector<ptr_type>& t)
          : visitable(t | std::views::transform([](ptr_type t) -> variable {
                        return {*t, {t->format()}};
                      }) |
                      std::ranges::to<std::vector>()) {}
      void load_arguments(const expr::arguments&);
      // FORMAT_DECL_IMPL();
      std::vector<ptr_type> types() const {
        return vector<parameter>::data() |
               std::views::transform(
                   [](const parameter& param) -> ptr_type { return &param.specs.type.value(); }) |
               std::ranges::to<std::vector>();
      }
    };
    struct signature;

    specifiers specs;
    decl::function::parameters params;
    scopes::function_scope* body;

    function(decltype(specs)&&, decltype(ident)&&, decltype(params)&&, decltype(body));
    function(const terms::operator_& name,
             ptr_type ret,
             const std::vector<ptr_type>& params,
             decltype(body) b)
        : specs(*ret),
          visitable(name),
          params(params),
          body(b) {}
    // FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };

  struct function::signature {
    cstring name;
    std::vector<ptr_type> types;

    bool operator==(const signature& other) const {
      return name == other.name &&
             std::ranges::all_of(std::views::zip(types, other.types), [](const auto& type_pair) {
               const auto& [t, other_t] = type_pair;
               return t == other_t;
             });
    }
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

  struct direct_conversion : public conversion_function {
    cr_type from_type;
    cr_type to_type;

    [[nodiscard]] bool is_convertible(cr_type t) const noexcept override { return from_type == t; }
    [[nodiscard]] cr_type to(cr_type) const noexcept override { return to_type; };

    direct_conversion(const decltype(body)&, cr_type, cr_type);
  };
  struct glob_conversion : public conversion_function {
    std::string description;
    using condition_t = std::function<bool(cr_type)>;
    using extractor_t = std::function<cr_type(cr_type)>;
    condition_t condition;
    extractor_t extractor;

    [[nodiscard]] bool is_convertible(cr_type t) const noexcept override { return condition(t); }
    [[nodiscard]] cr_type to(cr_type t) const noexcept override { return extractor(t); };

    glob_conversion(std::string, const decltype(body)&, const condition_t&, const extractor_t&);
  };
}; // namespace decl

struct stack_counter {
  // assembly_stack() = default;
  [[nodiscard]] size_t size() const { return stack_size; };
  void push() { stack_size++; };
  void pop(size_t times = 1) { stack_size -= times; }

private:
  size_t stack_size = 0;
};

using direct_conversion_store = hashmap<mangled_key, decl::direct_conversion>;
class conversion_store {
public:
  using key_type     = mangled_key;
  using value_type   = direct_conversion_store;

  conversion_store() = default;
  [[nodiscard]] bool is_convertible(const type&, const type&) const;
  [[nodiscard]] std::vector<ptr_type> get_convertible_types(const type&) const;
  [[nodiscard]] std::vector<const decl::conversion_function*> get_conversions(const type&) const;
  void emplace_direct(const decltype(decl::conversion_function::body)&, cr_type, cr_type);
  void emplace_glob(std::string&&,
                    const decltype(decl::conversion_function::body)&,
                    const decl::glob_conversion::condition_t&,
                    const decl::glob_conversion::extractor_t&);

private:
  direct_conversion_store m_direct_store;
  std::vector<decl::glob_conversion> m_glob_store;
};

using label_store = hashmap<mangled_key, decl::label>;
class frame : public node {
public:
  frame(const ast::decl::function*);
  const decl::function* function;
  stack_counter local_stack;
  label_store labels;
  stack<scopes::function_scope> local_scopes;

  local_scope& active_scope();
  [[nodiscard]] const local_scope& active_scope() const;
  void clear() noexcept;
  [[nodiscard]] bool is_declared(const ast::terms::identifier&) const noexcept;
  [[nodiscard]] decl::variable* get(const ast::terms::identifier&);
  [[nodiscard]] const decl::variable* get(const ast::terms::identifier&) const;
  void create_scope(const ast::scopes::function_scope&) noexcept;
  size_t destroy_scope() noexcept;
};

using stackframe_store = stack<frame>;

namespace selection {
  struct if_ : visitable<statement, if_> {
    terms::keyword keyword;
    expr::expression& condition;
    statement* block;
    statement* else_;

    if_(terms::keyword&&, expr::expression&, statement*, statement* = nullptr);
    // FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };

}; // namespace selection

namespace iteration {
  template <typename It>
  struct iteration : public visitable<statement, It> {
  private:
    iteration() = default;

  public:
    ~iteration() override = default;
    [[nodiscard]] virtual std::string condition_label() const;
    [[nodiscard]] virtual std::string exit_label() const;
    friend It;
  };

  struct while_ : visitable<iteration<while_>, while_> {
    terms::keyword keyword;
    expr::expression& condition;
    statement* body;
    while_(terms::keyword&&, expr::expression&, statement*);
    // FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };

  DERIVE_OK(statement, while_);
  static_assert(!std::is_abstract_v<while_>);

  struct for_ : visitable<iteration<for_>, for_> {
    terms::keyword keyword;
    decl::variable* start;
    expr::expression* condition;
    expr::expression* step;
    statement* body;
    for_(terms::keyword&& k,
         decl::variable* start_,
         expr::expression* condition_,
         expr::expression* step_,
         statement* block);
    // FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };

}; // namespace iteration

namespace jump {
  struct goto_ : visitable<statement, goto_> {
    terms::identifier term;
    goto_(const token&);
    // FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };
  struct break_ : visitable<statement, break_> {
    terms::keyword keyword;
    explicit break_(const token& token);
    // FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };

  struct continue_ : visitable<statement, continue_> {
    terms::keyword keyword;
    explicit continue_(const token& token);
    // FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };

  struct return_ : visitable<statement, return_> {
    terms::keyword keyword;
    expr::expression* expr;
    return_(terms::keyword k, expr::expression* expr_);
    // FORMAT_DECL_IMPL();
    cmm::location location() const override;
  };

} // namespace jump

namespace scopes {
  struct translation_unit : public declaration, visitable<global_scope, translation_unit> {
    using visitable::visitable;

    void clear() noexcept;
    [[nodiscard]] frame& active_frame() noexcept;
    [[nodiscard]] const frame& active_frame() const noexcept;
    template <typename Scope>
    [[nodiscard]] Scope&& active_scope() noexcept;

    // Frames
    void push_frame(const function_scope*);
    size_t pop_frame();

    template <typename T>
    bool is_declared(const ast::terms::identifier&) const noexcept;
    template <typename T>
    bool is_declarable(const ast::terms::identifier&) const noexcept;
    const decl::label* get_label(const ast::terms::identifier&) const;
    const decl::variable* get_variable(const ast::terms::identifier&) const;
    const decl::function* get_function(const function_signature&) const;
    void declare_function(const ast::decl::function*, bool = false);
    template <typename T>
    T* load_symbol(const ast::terms::identifier&);
    std::vector<ptr_type> get_conversions(cr_type t) {
      return m_conversions.get_convertible_types(t);
    }

    bool is_entry_point_defined() const noexcept;
    const decl::function* get_entry_point();
    void link_entry_point(const ast::decl::function*);
    [[nodiscard]] bool is_global_scope() const noexcept;
    bool in_main() const noexcept;

    // std::string format() const override;

  private:
    variable_store m_variables;
    function_store m_functions;
    stackframe_store m_stackframe;
    conversion_store m_conversions;
    global_scope m_global_scope;

    std::optional<const decl::function*> progressive_prefix_match(
        const std::vector<ptr_type>&,
        const std::vector<const decl::function*>&) const;
  };
  using namespace_ = translation_unit;
} // namespace scopes

using program = scopes::translation_unit;

#define TRACE_VISITOR(OBJ) \
  REGISTER_TRACE("{} visited {}", \
                 cpptrace::demangle(typeid(this).name()), \
                 cpptrace::demangle(typeid(OBJ).name()))

#define TERM_TYPES \
  ast::terms::literal, ast::terms::identifier, ast::terms::keyword, ast::terms::operator_, \
      ast::terms::storage, ast::terms::linkage, ast::terms::type

#define GLOBAL_TYPES ast::decl::function, ast::decl::variable

#define STATEMENT_TYPES \
  ast::decl::label, ast::iteration::for_, ast::iteration::while_, ast::selection::if_, \
      ast::jump::break_, ast::jump::continue_, ast::jump::goto_, ast::jump::return_

#define CHILDREN_TYPES ast::expr::arguments, ast::decl::function::parameters, ast::decl::specifiers

#define LITERAL_TYPES \
  \ ast::expr::string_literal, ast::expr::sint_literal, ast::expr::uint_literal, \
      ast::expr::char_literal, ast::expr::float_literal, ast::expr::false_literal, \
      ast::expr::true_literal
#define EXPRESSION_TYPES \
  ast::expr::binary_operator, ast::expr::unary_operator, ast::expr::call, ast::expr::literal, \
      ast::expr::identifier
#define NODE_TYPES STATEMENT_TYPES, EXPRESSION_TYPES, TERM_TYPES, CHILDREN_TYPES, GLOBAL_TYPES

struct ast_visitor : visitor<NODE_TYPES> {};
struct const_ast_visitor : const_visitor<NODE_TYPES> {};
} // namespace cmm::ast

#include "ast.inl"
