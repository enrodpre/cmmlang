#pragma once

#include "ast.hpp"
#include "common.hpp"
#include "expr.h"

#include <functional>
#include <revisited/visitor.h>
#include <type_traits>
#include <typeindex>

namespace cmm::ast {

enum class VisitorDirection : uint8_t {
  ParentToChild, // Top-down traversal
  ChildToParent, // Bottom-up traversal
  Horizontal     // Sibling traversal
};

enum class VisitorMode : uint8_t {
  TraverseAll,    // Visit every node encountered
  ConditionalOnly // Only visit nodes matching condition
};

template <VisitorDirection Dir, typename Ret = bool>
// requires(std::is_default_constructible_v<Ret>)
class generic_visitor : public revisited::RecursiveVisitor<node&, const node&> {
protected:
  using node_type  = node;
  using value_type = std::add_pointer_t<Ret>;

public:
  bool visit(node_type& node) override { return traverse(&node, false); }

  bool visit(const node_type& node) override {
    return traverse(const_cast<node_type*>(&node), false);
  }

  std::pair<bool, value_type> get_result() const { return m_result; }

protected:
  void set_result(value_type t_ret) { m_result = std::make_pair(true, t_ret); }

private:
  bool traverse(node_type* t_root, bool successful) {
    // m_count[std::type_index(typeid(*t_root))]++;

    if (visit_node(t_root)) {
      return true;
    }
    if (auto next = get_next(t_root)) {
      return traverse(next, successful);
    }
    return true;
  }

  virtual bool visit_node(node_type*) = 0;

  node_type* get_next(node_type* t_node) {
    if constexpr (Dir == VisitorDirection::ChildToParent) {
      return t_node->get_parent();
    } else if constexpr (Dir == VisitorDirection::ParentToChild) {
      // for (auto* child : t_node->children()) {
      //   if (child && traverse(child))
      //     return true;
      // }
      return nullptr;

    } else if constexpr (Dir == VisitorDirection::Horizontal) {
      auto* parent = t_node->get_parent();
      if (parent == nullptr) {
        return nullptr;
      }

      for (auto* sibling : parent->children()) {
        return sibling;
      }
      return nullptr;
    }
  };

protected:
  // Condition tracking for complex queries
  mutable std::unordered_map<std::type_index, int> m_count;
  std::pair<bool, value_type> m_result;
};

template <VisitorDirection Dir, typename Result>
class retriever_visitor : public generic_visitor<Dir, Result> {
  using node_type      = generic_visitor<Dir, Result>::node_type;
  using value_type     = std::add_pointer_t<std::add_const_t<Result>>;
  using condition_type = std::function<bool(const node_type*)>;

public:
  retriever_visitor(condition_type t_condition)
      : m_condition(t_condition) {}

protected:
  bool visit_node(node_type* t_node) override {
    bool result = m_condition(t_node);
    if (result) {
      generic_visitor<Dir, Result>::set_result(dynamic_cast<value_type>(t_node));
    }
    return result;
  }

  condition_type m_condition;
};

template <typename T>
constexpr auto is = [](const node& t_node) {
  return dynamic_cast<std::add_pointer_t<std::add_const_t<T>>>(t_node) != nullptr;
};

template <typename Ret>
struct parent_retriever_visitor : public retriever_visitor<VisitorDirection::ChildToParent, Ret> {
  parent_retriever_visitor()
      : retriever_visitor<VisitorDirection::ChildToParent, Ret>(
            is<typename retriever_visitor<VisitorDirection::ChildToParent, Ret>::value_type>) {}
};

template <typename Ret>
std::add_pointer_t<std::add_const_t<Ret>> find_parent(
    const node* t_node,
    const std::function<void()>& on_failure = []() {
      throw error(std::format("Couldnt find parent of type {}", typeid(Ret).name()));
    }) {
  parent_retriever_visitor<Ret> visitor{};
  t_node->accept(visitor);
  auto res = visitor.get_result();
  if (!res.first) {
    on_failure();
  }
  return res.second;
}

static_assert(std::is_const_v<std::remove_pointer_t<const translation_unit*>>);

ast::scope& get_scope(node*);

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
void print_node(const Node&);

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

#define TRACE_VISITOR(OBJ)                                                                     \
  REGISTER_TRACE("{} visited {}", demangle(typeid(this).name()), demangle(typeid(OBJ).name()))

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
