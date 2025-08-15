#pragma once

#include "common.hpp"
#include "macros.hpp"
#include "token.hpp"
#include <algorithm>
#include <cpptrace/utils.hpp>
#include <stdexcept>
#include <type_traits>

namespace cmm::ast {

template <class... Ts>
struct visitor : visitor<Ts>... {
  virtual ~visitor() = default;
};

template <class T>
struct visitor<T> : virtual visitor<> {
  virtual void visit(T&) = 0;
};

namespace detail {
  template <class T>
  struct visitor_no_warn : visitor<T> {
    void visit(T& t) override { visit((const T&)t); }
    virtual void visit(const T&) = 0;
  };
} // namespace detail

template <typename T>
struct visitor<const T> : detail::visitor_no_warn<T> {
  void visit(const T&) override = 0;
};

template <typename... Ts>
using const_visitor = visitor<const Ts...>;

namespace detail {
  struct dummy {};
  template <class T>
  inline void accept(visitor<>& v, T&) {
    throw std::domain_error(cpptrace::demangle(typeid(v).name()) +
                            " does not implement any visitor for " +
                            cpptrace::demangle(typeid(T).name()));
  }
  template <class T, class X, class... Z>
  inline void accept(visitor<>& v, T& p) {
    if (auto* av = dynamic_cast<visitor<X>*>(&v)) {
      av->visit(dynamic_cast<X&>(p));
    } else {
      accept<T, Z...>(v, p);
    }
  }
} // namespace detail

template <class B = void, class... T>
struct visitable : std::conditional_t<std::is_class_v<B>, B, visitable<>> {
  visitable()                                = default;
  ~visitable() override                      = default;
  visitable(const visitable&)                = default;
  visitable& operator=(const visitable&)     = default;
  visitable(visitable&&) noexcept            = default;
  visitable& operator=(visitable&&) noexcept = default;
  using parent = std::conditional_t<std::is_class_v<B>, B, visitable<>>;
  using parent::parent;

  // Inherit constructors explicitly
  template <typename... Args>
  visitable(Args&&... args)
    requires(
        std::is_constructible_v<std::conditional_t<std::is_class_v<B>, B, visitable<>>, Args...>)
      : std::conditional_t<std::is_class_v<B>, B, visitable<>>(std::forward<Args>(args)...) {}

  void accept(visitor<>& v) override { detail::accept<visitable, T...>(v, *this); };
  void accept(visitor<>& v) const override {
    detail::accept<const visitable, const T...>(v, *this);
  };
};

template <class B>
struct visitable<B> : std::conditional_t<std::is_class_v<B>, B, detail::dummy> {
  visitable()                                = default;
  virtual ~visitable()                       = default;
  visitable(const visitable&)                = default;
  visitable& operator=(const visitable&)     = default;
  visitable(visitable&&) noexcept            = default;
  visitable& operator=(visitable&&) noexcept = default;
  using parent = std::conditional_t<std::is_class_v<B>, B, detail::dummy>;
  using parent::parent;

  // Inherit constructors explicitly
  template <typename... Args>
  visitable(Args&&... args)
    requires(
        std::is_constructible_v<std::conditional_t<std::is_class_v<B>, B, detail::dummy>, Args...>)
      : std::conditional_t<std::is_class_v<B>, B, detail::dummy>(std::forward<Args>(args)...) {}

  virtual void accept(visitor<>&)       = 0;
  virtual void accept(visitor<>&) const = 0;
};

namespace detail {
  template <class base, class T>
  struct func_visitor_helper : visitor<T> {
    void visit(T& t) final { dynamic_cast<base&>(*this).f(t); }
  };
  template <class F, class... T>
  struct func_visitor final : func_visitor<func_visitor_helper<F, T...>, T>... {
    func_visitor(F&& f)
        : f(std::move(f)) {}
    func_visitor(const F& f)
        : f(f) {}
    template <class X>
    func_visitor& tryVisit(X& x) {
      x.accept(*this);
      return *this;
    }

  private:
    template <class, class> friend struct func_visitor_helper;
    F f;
  };

  template <class F>
  struct func_visitor<F> final : visitor<> {
    func_visitor(const F&) {}
  };
} // namespace detail

template <class... T, class F>
auto makeVisitor(F&& f) -> detail::func_visitor<std::decay_t<F>, T...> {
  return {std::forward<F>(f)};
}

template <typename Ret, typename... Ts>
struct fn_visitor;

template <typename Ret, typename Arg, typename... VisitableTypes>
struct fn_visitor<Ret(Arg), VisitableTypes...> : visitor<VisitableTypes...> {
  fn_visitor(Arg parameter, Ret initialResult = Ret())
      : parameter(parameter),
        result(initialResult) {}
  Arg parameter;
  Ret result;

  template <typename T>
  Ret accept(T& v) {
    v.accept(*this);
    return result;
  }
};

template <typename Arg, typename... VisitableTypes>
struct fn_visitor<void(Arg), VisitableTypes...> : visitor<VisitableTypes...> {
  fn_visitor(Arg parameter)
      : parameter(parameter) {}
  Arg parameter;

  template <typename T>
  void accept(T& v) {
    v.accept(*this);
  }
};

template <typename Ret, typename... VisitableTypes>
struct fn_visitor<Ret(), VisitableTypes...> : visitor<VisitableTypes...> {
  fn_visitor(Ret initialResult = Ret())
      : result(initialResult) {}
  Ret result;

  template <typename T>
  Ret accept(T& v) {
    v.accept(*this);
    return result;
  }
};

struct node : virtual visitable<>, public virtual displayable {
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

struct leaf : public virtual node {
  leaf() = default;
  leaf(cmm::location);
  leaf(const token&);

  cmm::location location() const final { return m_location; }

private:
  cmm::location m_location;
};

struct composite : public virtual node {

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

  void accept(visitor<>& vis) override {
    for (node* child : m_data) {
      child->accept(vis);
    }
  }
  void accept(visitor<>& vis) const override {
    for (const node* child : m_data) {
      child->accept(vis);
    }
  }

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
} // namespace cmm::ast
