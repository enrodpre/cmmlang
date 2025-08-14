#include "semantic.hpp"

namespace cmm {
//
// void semantics::load_program_semantics(ast::program* program) {
//   REGISTER_INFO("Loading semantics");
//   visitor visitor;
//   for (auto&& decl : *program) {
//     decl->accept(visitor);
//   }
//   REGISTER_INFO("Semantics loaded");
// }
//
// void semantics::load_expression_semantics(ast::expr::expression* expr) {
//   visitor visitor;
//   expr->accept(visitor);
// }
//
// semantics::visitor::visitor()
//     : v(ir::compilation_unit::instance()) {}
//
// void semantics::visitor::visit(const ast::expr::identifier& c) {
//   TRACE_VISITOR(c);
//   // const auto* var = v.table.get_variable(c.string());
//   // c.load_semantics(var->type, ast::expr::value_category_t::LVALUE);
//   c.semantics.is_constant_evaluable = false;
// }
// void semantics::visitor::visit(const ast ::expr ::float_literal& c) {
//   TRACE_VISITOR(c);
//   // c.load_semantics(c.type, ast::expr::value_category_t::RVALUE);
// };
//
// void semantics::visitor::visit(const ast ::expr ::sint_literal& c) {
//   TRACE_VISITOR(c);
//   // c.load_semantics(c.type, ast::expr::value_category_t::RVALUE);
// };
// void semantics::visitor::visit(const ast ::expr ::string_literal& c) {
//   TRACE_VISITOR(c);
//   // c.load_semantics(c.type, ast::expr::value_category_t::RVALUE);
// };
// void semantics::visitor::visit(const ast ::expr ::false_literal& c) {
//   TRACE_VISITOR(c);
//   c.load_semantics(c.type, ast::expr::value_category_t::RVALUE);
// };
// void semantics::visitor::visit(const ast ::expr ::true_literal& c) {
//   TRACE_VISITOR(c);
//   c.load_semantics(c.type, ast::expr::value_category_t::RVALUE);
// };
// void semantics::visitor::visit(const ast ::expr ::char_literal& c) {
//   TRACE_VISITOR(c);
//   // c.load_semantics(c.type, ast::expr::value_category_t::RVALUE);
// };
//
// namespace {
//   constexpr const ast::decl::function* get_function(ir::compilation_unit& v,
//                                                     const ast::identifier& name,
//                                                     const std::vector<ptr_type>& types) {
//     ast::decl::signature sig(name, types);
//     // if (const auto* fn = v.table.get_function(sig)) { return fn;
//     // }
//     throw_error<error_t::UNDECLARED_SYMBOL>(name);
//   }
//   constexpr void load_operator_semantics(ir::compilation_unit& v,
//                                          const ast::expr::expression& expr,
//                                          const ast::identifier& name,
//                                          const std::vector<ptr_type>& types) {
//     const auto* fn = get_function(v, name, types);
//     cr_type type   = fn->specs.type;
//     auto cat       = is_reference_v::operator()(type) ? ast::expr::value_category_t::LVALUE
//                                                       : ast::expr::value_category_t::RVALUE;
//     expr.load_semantics(&type, cat);
//     // expr.semantics.fn = fn;
//   }
// }; // namespace
// void semantics::visitor::visit(const ast::expr::unary_operator& c) {
//   // if (c.are_semantics_loaded()) {
//   //   return;
//   // }
//   // TRACE_VISITOR(c);
//   // c.expr.accept(*this);
//   // if (c.operator_.type == operator_t::post_dec || c.operator_.type == operator_t::post_inc) {
//   //   load_operator_semantics(v, c, c.operator_, {c.expr.type(), SINT_T});
//   // } else {
//   //   load_operator_semantics(v, c, c.operator_, {c.expr.type()});
//   // }
//   // load_operator_semantics(v, c, c.operator_, {c.expr.type()});
//   // c.semantics.is_constant_evaluable = c.expr.semantics.is_constant_evaluable;
// }
// void semantics::visitor::visit(const ast::expr::binary_operator& c) {
//   // if (c.are_semantics_loaded()) {
//   //   return;
//   // }
//   // TRACE_VISITOR(c);
//   // c.left.accept(*this);
//   // c.right.accept(*this);
//   // load_operator_semantics(v, c, c.operator_, {c.right.type(), c.left.type()});
//   // c.semantics.is_constant_evaluable =
//   //     c.left.semantics.is_constant_evaluable && c.right.semantics.is_constant_evaluable;
// }
// // void semantics::visitor::visit(const ast::expr::call& c) {
//   // if (c.are_semantics_loaded()) {
//   //   return;
//   // }
//   // TRACE_VISITOR(c);
//   // c.args.accept(*this);
//   // load_operator_semantics(v, c, c.ident, c.args.types());
//   // c.semantics.is_constant_evaluable = false;
// }

// void semantics::visitor::visit(const ast::expr::arguments& args) {
//   for (auto* arg : args) {
//     if (arg->are_semantics_loaded()) {
//       return;
//     }
//     arg->accept(*this);
//   }
// }
// shortener::visitor::visitor()
//     : v(ir::compilation_unit::instance()),
//       allocator(memory::Allocator::instance()),
//       res() {}
// // void semantics::visitor::visit(const ast::iteration::while_& c) {}
// // void semantics::visitor::visit(const ast::iteration::for_& c) {}
// // void semantics::visitor::visit(const ast::selection::if_& c) {}
// // void semantics::visitor::visit(const ast::jump::goto_& c) {}
// // void semantics::visitor::visit(const ast::jump::return_& c) {}
// ast::expr::expression* shortener::shorten_expression(ast::expr::expression* expr) {
//   visitor vis;
//   expr->accept(vis);
//   return vis.res;
// }
// void shortener::visitor::visit(ast::expr::identifier* c) {}
// void shortener::visitor::visit(ast::expr::literal* c) {}
// void shortener::visitor::visit(ast::expr::unary_operator* c) {}
// void shortener::visitor::visit(ast::expr::binary_operator* c) {
//   if (c->semantics.is_constant_evaluable) {
//     int res     = 0;
//     int left_i  = std::stoi(c->left.format());
//     int right_t = std::stoi(c->right.format());
//     if (c->operator_.type == operator_t::plus) {
//       res = left_i + right_t;
//     } else if (c->operator_.type == operator_t::star) {
//       res = left_i * right_t;
//     } else if (c->operator_.type == operator_t::minus) {
//       res = left_i - right_t;
//     } else if (c->operator_.type == operator_t::fslash) {
//       res = left_i / right_t;
//     } else {
//       NOT_IMPLEMENTED;
//     }
//     allocator.emplace<ast::expr::literal>(std::to_string(res), type_category_t::sint_t);
//   }
// }
// void shortener::visitor::visit(ast::expr::call* c) {}
}; // namespace cmm
