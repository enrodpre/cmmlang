#include "semantic.hpp"

#include "ir.hpp"

namespace cmm {

semantics::visitor::visitor()
    : v(ir::compilation_unit::instance()) {}

void semantics::visitor::visit(ast::expr::identifier& c) {
  const auto* var              = v.table.get_variable(c.term);
  c.semantics()->original_type = var->type;
}
void semantics::visitor::visit(ast::expr::literal& c) { c.semantics()->original_type = c.type; }
void semantics::visitor::visit(ast::expr::unary_operator& c) {
  c.expr.accept(*this);
  ir::function::signature_t sig(c.operator_.format(), c.expr.type());
  if (const auto* fn = v.table.get_function(sig)) {
    c.semantics()->original_type = fn->return_type;
  }
}
void semantics::visitor::visit(ast::expr::binary_operator& c) {
  c.left.accept(*this);
  c.right.accept(*this);
  ir::function::signature_t sig(c.operator_.format(), c.left.type(), c.right.type());
  if (const auto* fn = v.table.get_function(sig)) {
    c.semantics()->original_type = fn->return_type;
  }
}
void semantics::visitor::visit(ast::expr::call& c) {
  c.args.accept(*this);
  auto types = c.args | std::views::transform([this](const auto& expr) -> ptr_type {
                 return v.get_expression_type(*expr);
               }) |
               std::ranges::to<std::vector>();
  ir::function::signature_t sig(c.ident.value, types);
  c.semantics()->original_type = v.table.get_function(sig)->return_type;
}

void semantics::visitor::visit(ast::compound& c) {}
void semantics::visitor::visit(ast::decl::variable& c) {}
void semantics::visitor::visit(ast::decl::function& c) {}
void semantics::visitor::visit(ast::decl::label& c) {}
void semantics::visitor::visit(ast::iteration::while_& c) {}
void semantics::visitor::visit(ast::iteration::for_& c) {}
void semantics::visitor::visit(ast::selection::if_& c) {}
void semantics::visitor::visit(ast::jump::goto_& c) {}
void semantics::visitor::visit(ast::jump::return_& c) {}
}; // namespace cmm
