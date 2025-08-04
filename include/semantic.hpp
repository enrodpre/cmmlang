#pragma once

#include "ast.hpp"
#include "ir.hpp"

#define SET_PARENT_AND_VISIT(node, member) \
  node.member.set_parent(&(node)); \
  std::visit(this, (node).member);

#define RANGE_SET_PARENT_AND_VISIT(node, range) \
  for (auto& elem : node.range) { \
    elem.set_parent(&(node)); \
    std::visit(this, elem); \
  }

namespace cmm {

linkage_t parse_linkage(const ast::decl::specifiers& specs) {
  if (auto static_ = std::ranges::find_if(
          specs, [](const auto& spec) { return spec.type == token_t::static_; });
      static_ != specs.end()) {
    return linkage_t::internal;
  }
  return linkage_t::normal;
}

storage_t parse_storage(const ast::decl::specifiers& specs) {
  auto storages = specs.data() |
                  std::views::filter([](const auto& spec) { return spec.type.is_storage(); }) |
                  std::ranges::to<std::vector>();
  if (storages.size() == 0) {
    return storage_t::normal;
  }
  if (storages.size() == 1) {
    return storages[0].type.cast<storage_t>();
  }

  throw incompatible_token(storages[1].location(), storages[1].format(), storages[0].format());
}
namespace {
  constexpr type_category_t parse_enum_type(const token_t& token_type, bool unsigned_) {
    if (token_type == token_t::int_t) {
      return unsigned_ ? type_category_t::uint_t : type_category_t::sint_t;
    }
    return token_type.cast<type_category_t>();
  }
  cr_type parse_type(const ast::decl::specifiers& specs) {
    bool const_    = false;
    bool volatile_ = false;
    bool unsigned_ = false;
    std::optional<token_t> type_;
    for (const auto& t : specs) {
      if (t.type == token_t::const_) {
        const_ = true;
      } else if (t.type == token_t::volatile_) {
        volatile_ = true;
      } else if (t.type.is_type()) {
        type_.emplace(t.type);
      } else if (t.type == token_t::unsigned_) {
        unsigned_ = true;
      }
    }

    if (!type_.has_value()) {
      auto r = specs |
               std::views::transform([](const auto& spec) -> location { return spec.location(); }) |
               std::ranges::to<std::vector>();
      location l = std::ranges::fold_left(r.cbegin(), r.cend(), location(), std::plus<>());

      throw required_type(l);
    }
    return type::create_fundamental(parse_enum_type(type_.value(), unsigned_), const_, volatile_);
  }

} // namespace

namespace ir {
  struct mangled_name;
  struct compilation_unit;
} // namespace ir

struct semantics {
  struct visitor : cmm::visitor<STATEMENT_TYPES, EXPRESSION_TYPES> {
    visitor();
    ir::compilation_unit& v;
    void visit(ast::expr::identifier&) override;
    void visit(ast::expr::literal& c) override;
    void visit(ast::expr::unary_operator& c) override;
    void visit(ast::expr::binary_operator& c) override;
    void visit(ast::expr::call& c) override;
    void visit(ast::compound& c) override;
    void visit(ast::decl::variable& c) override;
    void visit(ast::decl::function& c) override;
    void visit(ast::decl::label& c) override;
    void visit(ast::iteration::while_& c) override;
    void visit(ast::iteration::for_& c) override;
    void visit(ast::selection::if_& c) override;
    void visit(ast::jump::goto_& c) override;
    void visit(ast::jump::return_& c) override;
  };
};
}; // namespace cmm
