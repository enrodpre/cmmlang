#pragma once

#include <algorithm>
#include <functional>
#include <optional>
#include <ranges>
#include <revisited/visitor.h>
#include <type_traits>
#include <utility>
#include <vector>

#include "common.hpp"
#include "macros.hpp"

namespace cmm::ast {

template <typename... Visitable>
using visitor = revisited::Visitor<std::add_lvalue_reference_t<Visitable>...>;
template <typename... Visitable>
using const_visitor = visitor<std::add_const_t<Visitable>...>;

template <typename B, typename T>
using visitable = revisited::DerivedVisitable<B, T>;

struct node : revisited::Visitable<node>, public displayable {
  ~node() override = default;
  node()           = default;
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
  virtual std::optional<cmm::location> location() const = 0;
  operator node*() { return static_cast<node*>(this); }
  std::string string() const override { return demangle(typeid(this).name()); }

private:
  mutable node* m_parent = nullptr;
};

template <typename T>
concept is_node = std::is_base_of_v<ast::node, T>;

template <typename T>
concept is_pointer_node = std::is_assignable_v<T, ast::node*>;

struct composite : visitable<composite, node> {

  composite() = default;
  ~composite() {
    for (const node* n : m_data) {
      // delete n;
    }
  }
  composite(std::vector<node*>&& v)
      : m_data(std::move(v)) {}

  void set_parent(node* n) const override {
    for (const node* a : m_data) {
      a->set_parent(n);
    }
  }
  [[nodiscard]] std::optional<cmm::location> location() const override {
    return std::ranges::fold_left_first(m_data | TRANSFORM(to_location) | FILTER(opt_has_value) |
                                            TRANSFORM(opt_to_value),
                                        std::plus<cmm::location>{})
        .value();
  }

  friend class check_visitor;
  friend class generic_node_visitor;

protected:
  template <typename... Nodes>
    requires(std::is_assignable_v<Nodes, node*>, ...)
  void add_all(Nodes... ns) {
    (add(ns), ...);
  }
  template <is_node T>
  void add(const std::vector<T*>& t) {
    for (auto&& elem : t) {
      add(elem);
    }
  }
  void add(node* n) {
    if (n != nullptr) {
      n->set_parent(this);
      m_data.push_back(n);
    }
  }

protected:
  std::vector<node*> m_data;
};

struct identifier;
struct literal;
} // namespace cmm::ast
