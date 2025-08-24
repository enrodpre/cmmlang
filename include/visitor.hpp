#pragma once

#include <optional>
#include <revisited/visitor.h>
#include <type_traits>

#include "common.hpp"
#include "macros.hpp"
#include "token.hpp"

namespace cmm::ast {

template <typename... Visitable>
using visitor = revisited::Visitor<std::add_lvalue_reference_t<Visitable>...>;
template <typename... Visitable>
using const_visitor = visitor<std::add_const_t<Visitable>...>;

template <typename T, typename B>
using visitable = revisited::DerivedVisitable<T, B>;

struct node : public revisited::Visitable<node>, public displayable {
  ~node() override = default;
  node(std::optional<cmm::location> l = {})
      : m_location(l) {}
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
  std::string string() const override { return demangle(typeid(this).name()); }
  virtual std::vector<node*> children() = 0;
  void initialize() {
    for (node* child : children()) {
      if (child != nullptr) {
        child->set_parent(this);
        m_location = m_location + child->location();
      }
    }
  }

private:
  mutable node* m_parent = nullptr;
  std::optional<cmm::location> m_location;
};

template <typename T>
concept is_node = std::is_base_of_v<ast::node, T>;

template <typename T>
concept is_pointer_node = std::is_assignable_v<T, ast::node*>;

struct identifier;
struct literal;

#define children_impl(CHILDREN)                                                   \
  std::vector<node*> children() override { return std::vector<node*>{CHILDREN}; }

#define to_node(OBJ) dynamic_cast<node*>(OBJ)
} // namespace cmm::ast
