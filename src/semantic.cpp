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

//
void semantics::load_program_semantics(ast::translation_unit* program) {
  REGISTER_INFO("Loading semantics");
  visitor visitor;
  for (auto&& decl : program->stmts) {
    decl->accept(visitor);
  }
  REGISTER_INFO("Semantics loaded");
}

void semantics::load_semantics(ast::expr::expression* e) {
  visitor vis;
  e->accept(vis);
}
semantics::visitor::visitor()
    : v(ir::compilation_unit::instance()) {}

void semantics::visitor::visit(const ast::expr::literal& lit) {
  switch (lit.category) {
    case ast::expr::literal_t::CHAR:
      lit.semantics()->original_type = cmm::type::create_fundamental(type_category_t::char_t);
      break;
    case ast::expr::literal_t::STRING:
      lit.semantics()->original_type = cmm::type::create_string(lit.value().size());
      break;
    case ast::expr::literal_t::SINT:
      lit.semantics()->original_type = cmm::type::create_fundamental(type_category_t::sint_t);
      break;
    case ast::expr::literal_t::UINT:
      lit.semantics()->original_type = cmm::type::create_fundamental(type_category_t::uint_t);
      break;
    case ast::expr::literal_t::FALSE:
    case ast::expr::literal_t::TRUE:
      lit.semantics()->original_type = cmm::type::create_fundamental(type_category_t::bool_t);
      break;
    case ast::expr::literal_t::FLOAT:
      lit.semantics()->original_type = cmm::type::create_fundamental(type_category_t::float_t);
      break;
  }

  switch (lit.category) {
    case ast::expr::literal_t::CHAR:
    case ast::expr::literal_t::SINT:
    case ast::expr::literal_t::UINT:
    case ast::expr::literal_t::FALSE:
    case ast::expr::literal_t::TRUE:
    case ast::expr::literal_t::FLOAT:
      lit.semantics()->value_category = value_category_t::PRVALUE;
      break;
    case ast::expr::literal_t::STRING:
      lit.semantics()->value_category = value_category_t::LVALUE;
      break;
  }
}

void semantics::visitor::visit(const ast::expr::identifier& c) {
  TRACE_VISITOR(c);
  const auto* var                      = v.ast->get_variable_declaration(c);
  c.semantics()->original_type         = var->specs.type.value();
  c.semantics()->value_category        = value_category_t::LVALUE;
  c.semantics()->is_constant_evaluable = false;
}

void semantics::visitor::visit(const ast::expr::unary_operator& c) {
  TRACE_VISITOR(c);
  c.expr.accept(*this);
  const builtin_operator_data* data = v.get_callable<builtin_operator_data>(c.operator_, {&c.expr});
  // if (c.operator_.value() == operator_t::post_inc || c.operator_.value() == operator_t::post_dec)
  // {
  //   load_operator_semantics(v, c, c.operator_, {c.expr.type(), SINT_T});
  // } else {
  //   load_operator_semantics(v, c, c.operator_, {c.expr.type()});
  // }
  auto* semantics                  = c.semantics();
  semantics->value_category        = get_value_category(data->ret);
  semantics->original_type         = data->ret;
  semantics->is_constant_evaluable = c.expr.semantics()->is_constant_evaluable;
}

void semantics::visitor::visit(const ast::expr::binary_operator& c) {
  TRACE_VISITOR(c);
  c.left.accept(*this);
  c.right.accept(*this);
  const builtin_operator_data* data =
      v.get_callable<builtin_operator_data>(c.operator_, {&c.left, &c.right});
  auto* semantics           = c.semantics();
  semantics->value_category = get_value_category(data->ret);
  semantics->original_type  = data->ret;
  c.semantics()->is_constant_evaluable =
      c.left.semantics()->is_constant_evaluable && c.right.semantics()->is_constant_evaluable;
}

void semantics::visitor::visit(const ast::expr::call& c) {
  TRACE_VISITOR(c);
  c.args.accept(*this);
  const auto* func                     = v.get_callable<ast::decl::function>(c.ident, {c.args});
  c.semantics()->original_type         = func->specs.type.value();
  c.semantics()->value_category        = get_value_category(func->specs.type.value());
  c.semantics()->is_constant_evaluable = false;
}

void semantics::visitor::visit(const ast::expr::arguments& args) {
  for (auto* arg : args) {
    arg->accept(*this);
  }
}

// void semantics::visitor::visit(ast::jump::goto_& c) {}
// void semantics::visitor::visit(ast::jump::return_& c) {}
}; // namespace cmm
