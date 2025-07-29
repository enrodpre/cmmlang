#pragma once

#include <stdexcept>
namespace cmm {

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
    throw std::domain_error(std::string(typeid(v).name()) + " does not implement any visitor for " +
                            typeid(T).name());
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
} // namespace cmm
