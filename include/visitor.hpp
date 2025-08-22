#pragma once

#include <cpptrace/utils.hpp>
#include <revisited/visitor.h>
#include <algorithm>
#include <type_traits>
#include <format>
#include <functional>
#include <optional>
#include <ranges>
#include <utility>
#include <vector>

#include "common.hpp"
#include "macros.hpp"
#include "static_type_info/type_id.h"

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
  virtual cmm::location location() const = 0;
  // virtual std::vector<node*> children() const = 0;
  operator node*() { return static_cast<node*>(this); }

private:
  mutable node* m_parent = nullptr;
};

template <typename T>
concept is_node = std::is_base_of_v<ast::node, T>;

template <typename T>
concept is_pointer_node = std::is_assignable_v<T, ast::node*>;

struct composite : revisited::DerivedVisitable<composite, node> {

  composite() = default;
  composite(std::vector<node*>&& v)
      : m_data(std::move(v)) {}
  template <is_node... Args>
  composite(Args&&...);
  // MOVABLE_CLS(composite) NOT_COPYABLE_CLS(composite)

  void set_parent(node* n) const override {
    for (const node* a : m_data) {
      a->set_parent(n);
    }
  }
  [[nodiscard]] cmm::location location() const override {
    return std::ranges::fold_left_first(
               m_data | std::views::transform([](const node* n) { return n->location(); }),
               std::plus<cmm::location>{})
        .value();
  }
  vector<node*> m_data;
  //
  // void accept(revisited::VisitorBase& vis) override {
  //   for (node* child : m_data) {
  //     child->accept(vis);
  //   }
  // }
  // void accept(revisited::VisitorBase& vis) const override {
  //   for (const node* child : m_data) {
  //     child->accept(vis);
  //   }
  // }

protected:
  template <typename... Nodes>
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
};

struct identifier;
struct literal;
} // namespace cmm::ast
