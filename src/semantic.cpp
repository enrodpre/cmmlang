#include "semantic.hpp"
#include "ast.hpp"
#include "expr.h"
#include "ir.hpp"
#include "types.hpp"

namespace cmm {
namespace {
  constexpr const ast::decl::function* get_function(ir::compilation_unit& v,
                                                    const ast::identifier& name,
                                                    const std::vector<ptr_type>& types) {
    ast::decl::signature sig(name, types);
    if (const auto* fn = v.ast->get_function(sig)) {
      return fn;
    }
    throw_error<compilation_error_t::UNDECLARED_SYMBOL>(name);
  }
  constexpr void load_operator_semantics(ir::compilation_unit& v,
                                         const ast::expr::expression& expr,
                                         const ast::operator_& name,
                                         const std::vector<ptr_type>& types) {
    const auto* fn = get_function(v, name, types);
    cr_type type   = fn->specs.type;
    auto cat       = is_reference_v::operator()(type) ? ast::expr::value_category_t::LVALUE
                                                      : ast::expr::value_category_t::RVALUE;
    expr.load_semantics(&type, cat);
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
  // const auto* var = v.table.get_variable(c.string());
  // c.load_semantics(var->type, ast::expr::value_category_t::LVALUE);
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
  // load_operator_semantics(v, c, c.operator_, {c.right.type(), c.left.type()});
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
void semantics::visitor::visit(ast::iteration::while_& c) {}
void semantics::visitor::visit(ast::iteration::for_& c) {}
void semantics::visitor::visit(ast::selection::if_& c) {
  c.condition.semantics.contextually_castable =
      &cmm::type::create_fundamental(type_category_t::bool_t);
}
void semantics::visitor::visit(ast::jump::goto_& c) {}
void semantics::visitor::visit(ast::jump::return_& c) {}
}; // namespace cmm
