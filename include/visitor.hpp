#pragma once

#include "common.hpp"
#include "token.hpp"
#include <algorithm>
#include <cpptrace/utils.hpp>
#include <stdexcept>

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
  using parent = std::conditional_t<std::is_class_v<B>, B, visitable<>>;
  using parent::parent;

  // Inherit constructors explicitly
  template <typename... Args>
  visitable(Args&&... args)
    requires(
        std::is_constructible_v<std::conditional_t<std::is_class_v<B>, B, visitable<>>, Args...>)
      : std::conditional_t<std::is_class_v<B>, B, visitable<>>(std::forward<Args>(args)...) {}

  ~visitable() override = default;
  void accept(visitor<>& v) override { detail::accept<visitable, T...>(v, *this); };
  void accept(visitor<>& v) const override {
    detail::accept<const visitable, const T...>(v, *this);
  };
};

template <class B>
struct visitable<B> : std::conditional_t<std::is_class_v<B>, B, detail::dummy> {
  using parent = std::conditional_t<std::is_class_v<B>, B, detail::dummy>;
  using parent::parent;

  // Inherit constructors explicitly
  template <typename... Args>
  visitable(Args&&... args)
    requires(
        std::is_constructible_v<std::conditional_t<std::is_class_v<B>, B, detail::dummy>, Args...>)
      : std::conditional_t<std::is_class_v<B>, B, detail::dummy>(std::forward<Args>(args)...) {}

  virtual ~visitable()                  = default;
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

struct node : virtual visitable<> {
  ~node() override = default;
  node()           = default;
  virtual const node* get_parent() const { return m_parent; }
  virtual void set_parent(node* parent_) const { m_parent = parent_; }
  virtual cmm::location location() const = 0;
  operator node*() { return static_cast<node*>(this); }

private:
  mutable node* m_parent = nullptr;
};

template <typename T>
concept is_node = std::is_base_of_v<ast::node, T>;

struct leaf : public virtual node {
  leaf() = default;
  leaf(cmm::location);
  leaf(const token&);

  cmm::location location() const override { return m_location; }

private:
  cmm::location m_location;
};

struct composite : public node, public virtual vector<node*> {
  virtual void add(node* n) { push_back(n); }

  composite() = default;
  composite(std::initializer_list<node*> init)
      : vector(init) {}
  template <typename... Args>
  composite(Args&&... args)
      : vector{std::forward<Args>(args)...} {}

  void set_parent(node* n) const override {
    set_parent(n);
    for (const node* a : *this) {
      a->set_parent(n);
    }
  }
  [[nodiscard]] cmm::location location() const override {
    return std::ranges::fold_left(
        *this | std::views::transform([](const node* n) { return n->location(); }),
        location(),
        std::plus<cmm::location>{});
  }
};

struct statement : public composite {
  using composite::composite;
};
} // namespace cmm::ast
