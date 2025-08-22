#include "semantic.hpp"

#include <format>
#include <vector>

#include "ast.hpp"
#include "common.hpp"
#include "expr.h"
#include "ir.hpp"
#include "lang.hpp"
#include "types.hpp"

namespace cmm {
namespace {
void load_operator_semantics(ir::compilation_unit& v,
                             const ast::expr::expression& expr,
                             const ast::operator_& name,
                             const std::vector<ptype>& types) {
  auto op          = v.get_operator_implementation(name, types);
  const auto* type = op.ret;
  auto cat         = is_reference_v::operator()(*type) ? ast::expr::value_category_t::LVALUE
                                                       : ast::expr::value_category_t::RVALUE;
  expr.load_semantics(type, cat);
  // expr.semantics.fn = fn;
}
}; // namespace
//
void semantics::load_program_semantics(ast::translation_unit* program) {
  REGISTER_INFO("Loading semantics");
  visitor visitor;
  for (auto&& decl : program->stmts) {
    decl->accept(visitor);
  }
  REGISTER_INFO("Semantics loaded");
}

void semantics::load_expression_semantics(ast::expr::expression* expr) {
  visitor visitor;
  expr->accept(visitor);
}

semantics::visitor::visitor()
    : v(ir::compilation_unit::instance()) {}

void semantics::visitor::visit(ast::expr::identifier& c) {
  TRACE_VISITOR(c);
  const auto* var = v.ast->get_variable(c.string());
  c.load_semantics(&var->specs.type.value(), ast::expr::value_category_t::LVALUE);
  c.semantics.is_constant_evaluable = false;
}
void semantics::visitor::visit(ast::expr::unary_operator& c) {
  if (c.are_semantics_loaded()) {
    return;
  }
  TRACE_VISITOR(c);
  c.expr.accept(*this);
  if (c.operator_.value() == operator_t::post_inc || c.operator_.value() == operator_t::post_dec) {
    // load_operator_semantics(v, c, c.operator_, {c.expr.type(), SINT_T});
  } else {
    // load_operator_semantics(v, c, c.operator_, {c.expr.type()});
  }
  // load_operator_semantics(v, c, c.operator_, {c.expr.type()});
  c.semantics.is_constant_evaluable = c.expr.semantics.is_constant_evaluable;
}
void semantics::visitor::visit(ast::expr::binary_operator& c) {
  if (c.are_semantics_loaded()) {
    return;
  }
  TRACE_VISITOR(c);
  c.left.accept(*this);
  c.right.accept(*this);
  load_operator_semantics(v, c, c.operator_, {&c.right.type(), &c.left.type()});
  c.semantics.is_constant_evaluable =
      c.left.semantics.is_constant_evaluable && c.right.semantics.is_constant_evaluable;
}
void semantics::visitor::visit(ast::expr::call& c) {
  if (c.are_semantics_loaded()) {
    return;
  }
  TRACE_VISITOR(c);
  c.args.accept(*this);
  // load_operator_semantics(v, c, c.ident, c.types());
  c.semantics.is_constant_evaluable = false;
}

void semantics::visitor::visit(ast::expr::arguments& args) {
  for (auto* arg : args) {
    if (arg->are_semantics_loaded()) {
      return;
    }
    arg->accept(*this);
  }
}
// void semantics::visitor::visit(ast::jump::goto_& c) {}
// void semantics::visitor::visit(ast::jump::return_& c) {}
}; // namespace cmm
