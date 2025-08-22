#pragma once

#include <type_traits>

#include "ast.hpp"
#include "visitor.hpp"

namespace cmm::ast {
namespace expr {
  struct expression;
} // namespace expr
struct node;
} // namespace cmm::ast

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
  static void load_program_semantics(ast::translation_unit*);
  static void load_expression_semantics(ast::expr::expression*);
  struct visitor : public ast::ast_visitor {
    visitor();
    ir::compilation_unit& v;
    void visit(ast::expr::binary_operator&) override;
    void visit(ast::expr::unary_operator&) override;
    void visit(ast::expr::call&) override;
    void visit(ast::expr::identifier&) override;
    void visit(ast::expr::arguments&) override;
  };

  // struct shortener {
  //   static ast::expr::expression* shorten_expression(ast::expr::expression*);
  //   struct visitor : public cmm::ptr_visitor<EXPRESSION_TYPES> {
  //     visitor();
  //     ir::compilation_unit& v;
  //     memory::Allocator& allocator;
  //     ast::expr::expression* res;
  //     void visit(ast::expr::identifier*) override;
  //     void visit(ast::expr::literal*) override;
  //     void visit(ast::expr::unary_operator*) override;
  //     void visit(ast::expr::binary_operator*) override;
  //     void visit(ast::expr::call*) override;
  //   };
  // };
}; // namespace cmm
} // namespace cmm
