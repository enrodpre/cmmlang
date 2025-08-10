#pragma once

#include "ast.hpp"
#include "ir.hpp"
#include <type_traits>

#define SET_PARENT_AND_VISIT(node, member) \
  node.member.set_parent(&(node)); \
  std::visit(this, (node).member);

#define RANGE_SET_PARENT_AND_VISIT(node, range) \
  for (auto& elem : node.range) { \
    elem.set_parent(&(node)); \
    std::visit(this, elem); \
  }

namespace cmm {

namespace ir {
  struct mangled_name;
  struct compilation_unit;
} // namespace ir

struct semantics {
  template <typename T>
    requires(std::is_base_of_v<ast::node, T>)
  static void load_node_semantics(T& t) {
    visitor v;
    return t.accept(v);
  }
  static void load_program_semantics(ast::program*);
  static void load_expression_semantics(ast::expr::expression*);
  struct visitor : public ast::const_ast_visitor {
    visitor();
    ir::compilation_unit& v;
    void visit(const ast::expr::identifier&) override;
    void visit(const ast::expr::literal&) override;
    void visit(const ast::expr::unary_operator&) override;
    void visit(const ast::expr::binary_operator&) override;
    void visit(const ast::expr::call&) override;
    // void visit(const ast::decl::variable&) override;
    // void visit(const ast::decl::function&) override;
    void visit(const ast::expr::call::arguments&) override;
    // void visit(ast::compound& c) override;
    // void visit(ast::decl::label& c) override;
    // void visit(ast::iteration::while_& c) override;
    // void visit(ast::iteration::for_& c) override;
    // void visit(ast::selection::if_& c) override;
    // void visit(ast::jump::goto_& c) override;
    // void visit(ast::jump::return_& c) override;
  };
};

struct shortener {
  static ast::expr::expression* shorten_expression(ast::expr::expression*);
  struct visitor : public cmm::ptr_visitor<EXPRESSION_TYPES> {
    visitor();
    ir::compilation_unit& v;
    memory::Allocator& allocator;
    ast::expr::expression* res;
    void visit(ast::expr::identifier*) override;
    void visit(ast::expr::literal*) override;
    void visit(ast::expr::unary_operator*) override;
    void visit(ast::expr::binary_operator*) override;
    void visit(ast::expr::call*) override;
  };
};
}; // namespace cmm
