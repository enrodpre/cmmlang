#pragma once

#include "lang.hpp"
#include "macros.hpp"
#include "visitor.hpp"
#include <utility>

namespace cmm ::ast {

class leaf;

template <typename Repr>
struct base_term : visitable<leaf> {
  base_term() = default;
  base_term(cmm::location l)
      : visitable(std::move(l)) {}
  base_term(const token& t)
      : visitable<leaf>(t) {}
  NOT_MOVABLE_CLS(base_term);
  COPYABLE_CLS(base_term);

  // [[nodiscard]] std::string repr(size_t) const override { return std::format("{}", value()); }
  // [[nodiscard]] std::string format() const override { return std::format("{}", value()); }
  operator const Repr&() const { return value(); }
  [[nodiscard]] virtual const Repr& value() const = 0;
  std::string string() const override { return std::format("{}", value()); }
};

struct keyword : visitable<base_term<keyword_t>, keyword> {
  using visitable::visitable;
  [[nodiscard]] const keyword_t& value() const override { return m_value; }

  // AST_LEAF
private:
  keyword_t m_value;
};
struct literal : visitable<base_term<std::string>, literal> {
  using visitable::visitable;
  literal(cmm::location l, std::string s)
      : visitable(std::move(l)),
        m_value(std::move(s)) {}
  [[nodiscard]] const std::string& value() const override { return m_value; }

  // AST_LEAF
private:
  std::string m_value;
};
struct operator_ : visitable<base_term<operator_t>, operator_> {
  operator_(const token& t)
      : visitable(t),
        m_value(token_data(t.type).cast<operator_t>()) {}
  operator_(const token& t, operator_t op)
      : visitable(t),
        m_value(op) {}
  std::string string() const override { return std::format("operator{}", m_value); }
  [[nodiscard]] const operator_t& value() const override { return m_value; }
  operator_data data() const { return {value()}; }
  // AST_LEAF

private:
  operator_t m_value;
};
template <typename T>
struct specifier : base_term<T> {
  using parent_t = base_term<T>;
  using parent_t::parent_t;
  specifier() = default;
  specifier(const token& x)
      : parent_t(x) {}
  friend T;
  // AST_LEAF
};
struct storage : visitable<specifier<storage_t>, storage> {
  storage() = default;
  storage(const token& t, storage_t l)
      : visitable(t),
        m_value(l) {}

  [[nodiscard]] const storage_t& value() const override { return m_value; }
  // AST_LEAF
private:
  storage_t m_value{};
};
struct linkage : visitable<specifier<linkage_t>, linkage> {
  linkage() = default;
  linkage(const token& t, linkage_t l)
      : visitable(t),
        m_value(l) {}
  [[nodiscard]] const linkage_t& value() const override { return m_value; }
  // AST_LEAF

private:
  linkage_t m_value{};
};
struct type : visitable<specifier<cr_type>, type> {
  type(cr_type t)
      : m_value(t) {}

  [[nodiscard]] cr_type value() const override { return m_value; }
  // AST_LEAF
private:
  cr_type m_value;
};
struct identifier : visitable<base_term<std::string>, identifier> {
  identifier() = default;
  identifier(const token& t)
      : visitable(t),
        m_value(t.value) {}
  identifier(std::string name)
      : m_value(std::move(name)) {}
  identifier(const operator_& op)
      : visitable(op.location()),
        m_value(std::format("operator{}", op.value())) {}

  [[nodiscard]] const std::string& value() const override { return m_value; }

  bool operator==(const identifier& other) const { return m_value == other.m_value; }

  // AST_LEAF

private:
  std::string m_value;
};
} // namespace cmm::ast
