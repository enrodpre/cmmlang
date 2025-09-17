#pragma once

#include "ast/tree.hpp"
#include "common.hpp"
#include "ast/expr.hpp"
#include "macros.hpp"

#include <cstddef>
#include <functional>
#include <revisited/visitor.h>
#include <type_traits>
#include <typeindex>

namespace cmm::ast {

enum class VisitorDirection : uint8_t {
  ParentToChild, // Top-down traversal
  ChildToParent, // Bottom-up traversal
};

template <VisitorDirection Dir, typename Ret = bool>
// requires(std::is_default_constructible_v<Ret>)
class generic_visitor : public revisited::RecursiveVisitor<node&, const node&> {
protected:
  using node_type = node;
  template <typename T, bool IsConst>
  using conditional_ptr_t =
      std::conditional_t<IsConst, std::add_pointer_t<std::add_const_t<T>>, std::add_pointer_t<T>>;

public:
  using mutable_value_type = std::add_pointer_t<Ret>;
  using const_value_type   = std::add_pointer_t<std::add_const_t<Ret>>;

  bool visit(node_type& node) override { return traverse(&node, false, std::false_type{}); }
  bool visit(const node_type& node) override { return traverse(&node, false, std::true_type{}); }

  auto& get_result() { return m_result; }
  const auto& get_result() const { return m_result; }

protected:
  void set_result(mutable_value_type t_ret) { m_result = std::make_pair(true, t_ret); }
  void set_result(const_value_type t_ret) const {
    m_result = std::make_pair(true, const_cast<mutable_value_type>(t_ret));
  }

  // Unified traverse that handles both const and non-const
  template <typename NodePtr, typename IsConstTag>
  bool traverse(NodePtr t_root, bool successful, IsConstTag is_const_tag) const {
    m_count[std::type_index(typeid(*t_root))]++;

    if (visit_node_dispatch(t_root, is_const_tag)) {
      return true;
    }

    if (auto next = get_next(t_root)) {
      return traverse(next, successful, is_const_tag);
    }

    return true;
  }
  // Dispatch to the correct visit_node based on constness
  bool visit_node_dispatch(node_type* t_node, std::false_type) const {
    return const_cast<generic_visitor*>(this)->visit_node(t_node);
  }

  bool visit_node_dispatch(const node_type* t_node, std::true_type) const {
    return visit_node_const(t_node);
  }

  virtual bool visit_node(node_type*)                   = 0;
  virtual bool visit_node_const(const node_type*) const = 0;

  // Template get_next that preserves constness
  template <typename NodePtr>
  NodePtr get_next(NodePtr t_node) const {
    if constexpr (Dir == VisitorDirection::ChildToParent) {
      return t_node->get_parent();
    } else if constexpr (Dir == VisitorDirection::ParentToChild) {
      return nullptr; // Simplified for this example
    }
    return nullptr;
  }

  mutable std::unordered_map<std::type_index, int> m_count;
  mutable std::pair<bool, mutable_value_type> m_result;
};

template <VisitorDirection Dir, typename Result>
class retriever_visitor : public generic_visitor<Dir, Result> {
  using base_type      = generic_visitor<Dir, Result>;
  using node_type      = typename base_type::node_type;
  using condition_type = std::function<bool(const node_type*)>;

public:
  using mutable_value_type = typename base_type::mutable_value_type;
  using const_value_type   = typename base_type::const_value_type;

  retriever_visitor(condition_type t_condition)
      : m_condition(t_condition) {}

protected:
  bool visit_node(node_type* t_node) override {
    bool result = m_condition(t_node);
    if (result) {
      // Try to dynamic_cast to the target type (non-const)
      if (auto casted = dynamic_cast<mutable_value_type>(t_node)) {
        base_type::set_result(casted);
      }
    }
    return result;
  }

  bool visit_node_const(const node_type* t_node) const override {
    bool result = m_condition(t_node);
    if (result) {
      // Try to dynamic_cast to the target type (const)
      if (auto casted = dynamic_cast<const_value_type>(t_node)) {
        base_type::set_result(casted);
      }
    }
    return result;
  }

  condition_type m_condition;
};

template <typename T>
constexpr auto is = [](const node* t_node) {
  return dynamic_cast<std::add_pointer_t<std::add_const_t<T>>>(t_node) != nullptr;
};

template <typename Ret>
struct parent_retriever_visitor : public retriever_visitor<VisitorDirection::ChildToParent, Ret> {
  parent_retriever_visitor()
      : retriever_visitor<VisitorDirection::ChildToParent, Ret>(is<Ret>) {}
};

template <typename Ret>
std::add_pointer_t<Ret> find_parent(
    node* t_node,
    const std::function<void()>& on_failure = []() {
      cpptrace::stacktrace::current().print();
      throw error(
          std::format("Couldn't find parent {} of {}", DEMANGLE(decltype(t_node)), DEMANGLE(Ret)));
    }) {
  parent_retriever_visitor<Ret> visitor{};
  t_node->accept(visitor);
  auto res = visitor.get_result();
  if (!res.first) {
    on_failure();
  }
  // Return non-const pointer when input was non-const
  return res.second;
}

template <typename Ret>
std::add_pointer_t<std::add_const_t<Ret>> find_parent(
    const node* t_node,
    const std::function<void()>& on_failure = []() {
      throw error(std::format(
          "Couldn't find parent of {} of {}", DEMANGLE(decltype(t_node)), DEMANGLE(Ret)));
    }) {
  parent_retriever_visitor<Ret> visitor{};
  t_node->accept(visitor);
  auto res = visitor.get_result();
  if (!res.first) {
    on_failure();
  }
  // Ensure const correctness - if input was const, return const
  return static_cast<std::add_pointer_t<std::add_const_t<Ret>>>(res.second);
}

static_assert(std::is_const_v<std::remove_pointer_t<const translation_unit*>>);

ast::scope& get_scope(node*);

struct ast_printer : public revisited::RecursiveVisitor<const node&> {
  ast_printer() = default;

  std::string get_result() const { return m_result.str(); }

protected:
  bool visit(const node& n) final {
    print_node(n);
    traverse_children(n);
    return false; // keep traversing
  }

private:
  void print_node(const node& n) {
    m_result << std::string(static_cast<size_t>(m_depth * 2), ' ') << n.string() << "\n";
  }

  void traverse_children(const node& n) {
    ++m_depth;
    for (const auto* child : n.children()) {
      if (child != nullptr) {
        child->accept(*this);
      }
    }
    --m_depth;
  }

  mutable int m_depth = 0;
  mutable std::stringstream m_result;
};
template <VisitorDirection Dir>
class operator_visitor : public generic_visitor<Dir, bool> {
  using node_type           = generic_visitor<Dir, bool>::node_type;
  using should_operate_type = std::function<bool(const node_type*)>;
  using should_stop_type    = std::function<bool(const node_type*)>;
  using operation_type      = std::function<bool(node_type*)>;

  operator_visitor(should_operate_type t_should_operate,
                   should_stop_type t_should_stop,
                   operation_type t_operation)
      : m_operate_condition(t_should_operate),
        m_stop_condition(t_should_stop),
        m_operation(t_operation) {}

  std::pair<bool, bool> visit_node(node_type* t_root) override {
    bool should_operate = m_operate_condition(t_root);
    bool should_stop    = m_stop_condition(t_root);
    bool successful     = true;
    if (should_operate) {
      successful = m_operation(t_root);
    }
    return std::make_pair(should_stop, successful);
  }

protected:
  should_operate_type m_operate_condition;
  should_stop_type m_stop_condition;
  operation_type m_operation;
};

template <typename Node>
  requires(std::is_base_of_v<node, Node>)
inline void print_node(const Node& t_node) {
  ast::ast_printer printer;
  t_node.accept(printer);

  fmt::print("{}", printer.get_result());
}

class ConditionBuilder {
public:
  // Type-based conditions
  template <typename T>
  static std::function<bool(const node*)> isType() {
    return [](const node* n) { return dynamic_cast<const T*>(n) != nullptr; };
  }

  template <typename T>
  static std::function<bool(const node*)> isDerivedFrom() {
    return [](const node* n) { return dynamic_cast<const T*>(n) != nullptr; };
  }

  // Positional conditions
  template <typename T>
  static std::function<bool(const node*)> nthOfType(int n) {
    return [n](const node* node) {
      static thread_local std::unordered_map<std::type_index, int> counts;
      auto type_idx = std::type_index(typeid(T));

      if (dynamic_cast<const T*>(node)) {
        counts[type_idx]++;
        return counts[type_idx] == n;
      }
      return false;
    };
  }

  template <typename T>
  static std::function<bool(const node*)> firstOfType() {
    return nthOfType<T>(1);
  }

  // Combinators
  static std::function<bool(const node*)> andCondition(const std::function<bool(const node*)>& a,
                                                       const std::function<bool(const node*)>& b) {
    return [a, b](const node* n) { return a(n) && b(n); };
  }

  static std::function<bool(const node*)> orCondition(const std::function<bool(const node*)>& a,
                                                      const std::function<bool(const node*)>& b) {
    return [a, b](const node* n) { return a(n) || b(n); };
  }
};

#define TRACE_VISITOR(OBJ)                                \
  REGISTER_TRACE("{} visited {}",                         \
                 cpptrace::demangle(typeid(this).name()), \
                 cpptrace::demangle(typeid(OBJ).name()))

#define TERM_TYPES                                                                \
  ast::literal, ast::identifier, ast::keyword, ast::operator_, ast::storage_spec, \
      ast::linkage_spec, ast::type_spec

#define GLOBAL_TYPES ast::decl::function, ast::decl::variable

#define STATEMENT_TYPES                                                                \
  ast::decl::label, ast::iteration::for_, ast::iteration::while_, ast::selection::if_, \
      ast::jump::break_, ast::jump::continue_, ast::jump::goto_, ast::jump::return_,   \
      ast::decl::function::definition, ast::decl::block

#define CHILDREN_TYPES ast::decl::specifiers, ast::expr::arguments

#define EXPRESSION_TYPES                                                                         \
  ast::expr::binary_operator, ast::expr::unary_operator, ast::expr::call, ast::expr::identifier, \
      ast::expr::literal

#define NODE_TYPES STATEMENT_TYPES, EXPRESSION_TYPES, TERM_TYPES, CHILDREN_TYPES, GLOBAL_TYPES

struct ast_visitor : public visitor<NODE_TYPES> {
  void visit(ast::expr::identifier&) override;
  void visit(ast::decl::specifiers&) override;
  void visit(ast::decl::function::definition&) override;
  void visit(ast::literal&) override;
  void visit(ast::keyword&) override;
  void visit(ast::storage_spec&) override;
  void visit(ast::type_spec&) override;
  void visit(ast::linkage_spec&) override;
  void visit(ast::expr::arguments&) override;
  void visit(ast::identifier&) override;
  void visit(ast::expr::unary_operator& c) override;
  void visit(ast::expr::literal& c) override;
  void visit(ast::expr::binary_operator& c) override;
  void visit(ast::expr::call& c) override;
  void visit(ast::decl::variable& c) override;
  void visit(ast::decl::function& c) override;
  void visit(ast::decl::label& c) override;
  void visit(ast::decl::block&) override;
  void visit(ast::iteration::while_& c) override;
  void visit(ast::iteration::for_& c) override;
  void visit(ast::selection::if_& c) override;
  void visit(ast::jump::goto_& c) override;
  void visit(ast::jump::return_& c) override;
  void visit(ast::jump::continue_& c) override;
  void visit(ast::operator_&) override;
  void visit(ast::jump::break_& c) override;
}; // namespace cmm::ast

struct const_ast_visitor : const_visitor<NODE_TYPES> {
  void visit(const ast::expr::identifier&) override;
  void visit(const ast::decl::specifiers&) override;
  void visit(const ast::decl::function::definition&) override;
  void visit(const ast::decl::block&) override;
  void visit(const ast::literal&) override;
  void visit(const ast::keyword&) override;
  void visit(const ast::storage_spec&) override;
  void visit(const ast::type_spec&) override;
  void visit(const ast::operator_&) override;
  void visit(const ast::linkage_spec&) override;
  void visit(const ast::identifier&) override;
  void visit(const ast::expr::arguments&) override;
  void visit(const ast::expr::literal& c) override;
  void visit(const ast::expr::unary_operator& c) override;
  void visit(const ast::expr::binary_operator& c) override;
  void visit(const ast::expr::call& c) override;
  void visit(const ast::decl::variable& c) override;
  void visit(const ast::decl::function& c) override;
  void visit(const ast::decl::label& c) override;
  void visit(const ast::iteration::while_& c) override;
  void visit(const ast::iteration::for_& c) override;
  void visit(const ast::selection::if_& c) override;
  void visit(const ast::jump::goto_& c) override;
  void visit(const ast::jump::return_& c) override;
  void visit(const ast::jump::continue_& c) override;
  void visit(const ast::jump::break_& c) override;
};

} // namespace cmm::ast
