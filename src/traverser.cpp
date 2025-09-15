#include <cstddef>
#include <format>
#include <magic_enum/magic_enum_flags.hpp>
#include <magic_enum/magic_enum_fuse.hpp>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>

#include "asm.hpp"
#include "ast.hpp"
#include "ast_visitor.hpp"
#include "common.hpp"
#include "expr.h"
#include "ir.hpp"
#include "lang.hpp"
#include "traverser.hpp"

namespace cmm::ir {

using namespace ast;
using namespace assembly;

ast_traverser::ast_traverser(compilation_unit& cunit)
    : m_context(cunit),
      ast() {}

global_visitor::global_visitor(ast_traverser* gen_)
    : gen(gen_) {}

void ast_traverser::generate_program(translation_unit& p) {
  ast = &p;
  global_visitor visitor{this};
  // REGISTER_INFO("Program:\n{}", p.repr(0));
  for (auto* decl : p.stmts) {
    decl->accept(visitor);
  }
}

operand* ast_traverser::generate_expr(expr::expression& expr, operand* reg_, intent_t t_intent) {
  if (reg_ == nullptr) {
    reg_ = m_context.regs.get(register_t::ACCUMULATOR);
  }

  auto visitor = expression_visitor{this, reg_, t_intent};
  expr.accept(visitor);
  return visitor.out;
}

void ast_traverser::generate_statement(statement* stmt) {
  statement_visitor visitor(this);
  stmt->accept(visitor);
}

void ast_traverser::generate_statements(decl::block& elems) {
  for (auto* elem : elems.stmts) {
    generate_statement(elem);

    m_context.last.operator_.reset();

    // If exit is generated that means the rest of the code is unrecheable
    if (m_context.current_phase == Phase::EXITING ||
        m_context.current_phase == Phase::PRUNING_FULL) {

      if (m_context.current_phase == Phase::PRUNING_FULL) {
        m_context.current_phase = Phase::GENERATING;
      }

      break;
    }
  }
}

void ast_traverser::generate_condition(expr::expression& cond, const std::string& label) {
  generate_expr(cond);
  operator_data last_op       = m_context.last.operator_.value();
  comparison_data comp        = last_op.comparison.value();
  comparison_data inverted    = comp.inverse;
  instruction_t inverted_jump = inverted.jump();
  m_context.jump(inverted_jump, label);
}

void ast_traverser::begin_scope(decl::block& stmts) { ast->active_frame()->create_scope(stmts); }

void ast_traverser::end_scope() {
  size_t ditched = ast->active_frame()->destroy_scope();

  if (ditched > 0) {
    m_context.move_rsp(ditched);
  }
}

template <typename Jump>
void ast_traverser::generate_continue_break(const Jump& node) {

  const auto* it = find_parent<ast::iteration::iteration>(&node, [node]() {
    if constexpr (std::is_same_v<jump::continue_, Jump>) {
      THROW(INVALID_CONTINUE, node);
    } else if constexpr (std::is_same_v<jump::break_, Jump>) {
      THROW(INVALID_BREAK, node);
    }
  });

  auto labels    = it->labels();
  if constexpr (std::is_same_v<jump::continue_, Jump>) {
    m_context.jump(labels.first);
  } else if constexpr (std::is_same_v<jump::break_, Jump>) {
    m_context.jump(labels.second);
  }
  m_context.current_phase = Phase::PRUNING_FULL;
}

template void ast_traverser::generate_continue_break<jump::continue_>(const jump::continue_&);
template void ast_traverser::generate_continue_break<jump::break_>(const jump::break_&);

template <bool IsGlobal>
void ast_traverser::generate_variable_decl(decl::variable* vardecl) {
  if (ast->is_declarable(vardecl->ident)) {
    THROW(ALREADY_DECLARED_SYMBOL, vardecl->ident);
  }

  operand* reg_ = nullptr;
  if (auto* defined = vardecl->init) {
    reg_ = generate_expr(*defined);
  } else {
    reg_ = m_context.move_immediate(m_context.regs.get(register_t::ACCUMULATOR), "0");
  }

  if constexpr (IsGlobal) {
    auto* addr = ast->declare_variable(vardecl);
    m_context.move(addr, reg_);
  } else {
    auto b = m_context.asmgen.begin_comment_block("init variable {}", vardecl->ident.value());
    ast->declare_variable(vardecl);
    m_context.push(reg_);
  }
}

void global_visitor::visit(decl::variable& vardecl) { gen->generate_variable_decl<true>(&vardecl); }

void global_visitor::visit(decl::function& func) {
  REGISTER_INFO("Generating function {}", func.ident.value());
  if (gen->ast->is_declarable(func.ident)) {
    THROW(ALREADY_DECLARED_SYMBOL, func.ident);
  }
  gen->ast->declare_function(&func);
  if (func.body) {
    gen->ast->define_function(&func);
  }
}

expression_visitor::expression_visitor(ast_traverser* t, operand* o, intent_t t_intent)
    : gen(t),
      in(o),
      out(o),
      intent(t_intent) {}

#define gen_op(IN) value_or(generate<operand*>(in))

void expression_visitor::visit(expr::call& expr_call) {
  out = gen->m_context.call_function(expr_call.ident, expr_call.args).gen_op(in);
}

void expression_visitor::visit(expr::binary_operator& binop) {
  auto op = binop.operator_.value();
  out     = gen->m_context.call_builtin_operator(binop.operator_, {&binop.left, &binop.right});
  gen->m_context.last.operator_.emplace(op);
}

void expression_visitor::visit(expr::unary_operator& unary) {
  auto op = unary.operator_.value();
  out     = gen->m_context.call_builtin_operator(unary.operator_, {&unary.expr});
  gen->m_context.last.operator_.emplace(op);
}

void expression_visitor::visit(ast ::expr ::literal& c) {
  switch (c.category) {
    case ast::expr::literal_t::STRING:
    case ast::expr::literal_t::CHAR:
      {
        const auto& pair = gen->m_context.reserve_constant(c.value());
        out              = gen->m_context.move(in, pair);
      }
      break;
    case ast::expr::literal_t::FALSE:
      out = gen->m_context.move_immediate(in, "0");
      break;
    case ast::expr::literal_t::TRUE:
      out = gen->m_context.move_immediate(in, "1");
      break;
    case ast::expr::literal_t::SINT:
    case ast::expr::literal_t::UINT:
    case ast::expr::literal_t::FLOAT:
      out = gen->m_context.move_immediate(in, c.value());
      break;
  }
}

void expression_visitor::visit(expr::identifier& ident) {
  const auto& opt = gen->m_context.regs.find_var(ident);
  if (opt) {
    out = opt.value();
    return;
  }

  const auto& [var, addr] = gen->ast->get_variable(ident);
  auto b                  = gen->m_context.asmgen.begin_comment_block("loading {}", ident.value());
  if (magic_enum::enum_flags_test(ident.value_category(), value_category_t::LVALUE)) {
    out = gen->m_context.lea(out, addr);
  } else {
    out = gen->m_context.move(out, addr);
  }
}

statement_visitor::statement_visitor(ast_traverser* gen_)
    // : expression_visitor(gen_,
    //                      gen_->m_context.regs.get(registers::ACCUMULATOR),
    //                      intent_t::LOAD_VARIABLE_VALUE),
    : gen(gen_) {}

void statement_visitor::visit(ast::decl::block& scope) {
  // Create scope
  gen->begin_scope(scope);

  // Handle every element within scope
  gen->generate_statements(scope);

  // Discard scope's elements
  gen->end_scope();
}

void statement_visitor::visit(expr::expression& expr) { gen->generate_expr(expr); }

void statement_visitor::visit(decl::variable& vardecl) {
  gen->generate_variable_decl<false>(&vardecl);
}

void statement_visitor::visit(decl::label& label_) {
  if (gen->ast->is_declarable(label_.ident)) {
    THROW(ALREADY_DECLARED_SYMBOL, label_);
  }

  const auto* fn = find_parent<ast::decl::function::definition>(label_);
  if (!fn) {
    THROW(LABEL_IN_GLOBAL, label_.ident);
  }
  if (fn->labels.contains(label_.ident)) {
    THROW(ALREADY_DECLARED_SYMBOL, label_.ident);
  }
  // res.second->labels.insert()
  gen->m_context.label(label_.ident.value());

  if (gen->m_context.current_phase == Phase::PRUNING_LABEL &&
      gen->m_context.last.pruning_label->value() == label_.ident.value()) {
    gen->m_context.current_phase = Phase::GENERATING;
  }
}

void statement_visitor::visit(decl::function&) {} // UNREACHABLE("not implemented"); }

void statement_visitor::visit(iteration::for_& for_) {
  if (for_.start != nullptr) {
    // Generate init statement
    gen->generate_statement(for_.start);
  }

  const auto& [condition_label, exit_label] = for_.labels();

  gen->m_context.asmgen.write_label(condition_label);
  // Checking condition
  if (for_.condition) {
    for_.condition->current_conversor = any_to_bool;
    gen->generate_condition(*for_.condition, for_.labels().second);
  }

  // If true execute body and check cond again
  if (auto* body = for_.body) {
    gen->generate_statement(body);
  }

  if (auto* step = for_.step) {
    gen->generate_expr(*step);
  }
  gen->m_context.jump(condition_label);
  gen->m_context.asmgen.write_label(exit_label);
}

void statement_visitor::visit(iteration::while_& while_) {
  const auto& [condition_label, exit_label] = while_.labels();
  gen->m_context.asmgen.write_label(condition_label);

  // Checking condition
  while_.condition->current_conversor = any_to_bool;
  gen->generate_condition(while_.condition, while_.labels().second);

  // If true execute body and check cond again
  if (auto* body = while_.body) {
    gen->generate_statement(body);
  }
  gen->m_context.jump(condition_label);
  gen->m_context.asmgen.write_label(exit_label);
}

void statement_visitor::visit(selection::if_& if_) {
  // Checking condition
  auto a                           = gen->m_context.asmgen.begin_comment_block("if condition");

  if_.condition->current_conversor = any_to_bool;
  gen->generate_condition(if_.condition, "else block");
  a.end();

  std::string_view after_if_label = "after_if";
  if (auto* block = if_.block) {
    auto b = gen->m_context.asmgen.begin_comment_block("if block");
    gen->generate_statement(block);
  }

  gen->m_context.asmgen.write_label(after_if_label);
  if (auto* else_ = if_.else_) {
    auto b = gen->m_context.asmgen.begin_comment_block("else block");
    gen->generate_statement(else_);
  }
}

void statement_visitor::visit(jump::break_& break_) {
  gen->generate_continue_break<jump::break_>(break_);
}

void statement_visitor::visit(jump::continue_& continue_) {
  auto b = gen->m_context.asmgen.begin_comment_block("continue");
  gen->generate_continue_break<jump::continue_>(continue_);
  b.end();
}

void statement_visitor::visit(jump::goto_& goto_) {
  gen->m_context.jump(goto_.term.value());
  if (!gen->ast->is_declared<decl::label>(goto_.term)) {
    gen->m_context.current_phase      = Phase::PRUNING_LABEL;
    gen->m_context.last.pruning_label = &goto_.term;
  }
}

void statement_visitor::visit(decl::function::definition& def) { gen->generate_statements(def); }

void statement_visitor::visit(jump::return_& ret) {
  if (gen->m_context.current_phase == Phase::PRUNING_LABEL) {
    return;
  }

  if (gen->ast->is_global_scope()) {
    THROW(RETURN_IN_GLOBAL, ret);
  }

  operand* op = nullptr;
  if (ret.expr != nullptr) {
    const ast::decl::function* fn = find_parent<ast::decl::function>(ret);
    op = gen->generate_expr(*translation_unit::bind_expression(ret.expr, fn->specs.type));
  } else {
    op = gen->m_context.zero(gen->m_context.regs.get(register_t::ACCUMULATOR));
  }

  if (!gen->ast->in_main()) {
    // When there is a return, the rest of the scope should not be generated
    gen->m_context.current_phase = Phase::PRUNING_FULL;
    gen->m_context.ret();
  } else {
    gen->m_context.exit(op);
    gen->m_context.current_phase = Phase::EXITING;
  }
}

} // namespace cmm::ir
