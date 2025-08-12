#include "asm.hpp"
#include "ast.hpp"
#include "common.hpp"
#include "ir.hpp"
#include <libassert/assert.hpp>
#include <traverser.hpp>

#include "lang.hpp"
#include "semantic.hpp"
#include "types.hpp"

namespace cmm::ir {

using namespace ast;
using namespace assembly;
using intents::intent_t;

ast_traverser::ast_traverser(compilation_unit& cunit)
    : m_context(cunit) {}

global_visitor::global_visitor(ast_traverser* gen_)
    : gen(gen_) {}

void ast_traverser::generate_program(const ast::program& p) {
  global_visitor visitor{this};
  REGISTER_INFO("Program:\n{}", p.repr(0));
  for (const auto* decl : p) {
    decl->accept(visitor);
  }
}

operand* ast_traverser::generate_expr(const expr::expression& expr,
                                      intents::intent_t intent_,
                                      operand* reg_) {

  auto visitor = expression_visitor{this, reg_, intent_};
  semantics::load_node_semantics(expr);
  expr.accept(visitor);
  return visitor.out;
}

operand* ast_traverser::generate_expr(const ast::expr::expression& expr,
                                      ir::intents::intent_t intent) {
  return generate_expr(expr, intent, m_context.regs.get(registers::ACCUMULATOR));
};

operand* ast_traverser::generate_expr(const ast::expr::expression& expr) {
  return generate_expr(
      expr, intent_t::LOAD_VARIABLE_VALUE, m_context.regs.get(registers::ACCUMULATOR));
}

assembly::operand* ast_traverser::generate_expr(const ast::expr::expression& expr,
                                                cr_type t,
                                                assembly::operand* o) {
  return generate_expr(expr,
                       cmm::types::is_indirect_v::operator()(t)
                           ? intents::intent_t::LOAD_VARIABLE_ADDRESS
                           : intents::intent_t::LOAD_VARIABLE_VALUE,
                       o);
}
void ast_traverser::generate_statement(const statement* stmt) {
  // m_context.src_location = stmt->location();
  statement_visitor visitor(this);
  stmt->accept(visitor);
  m_context.regs.parameters.reset();
}

void ast_traverser::generate_statements(const ast::scope::block& elems) {
  for (const auto* elem : elems) {
    m_context.current_statement = elem;
    generate_statement(elem);

    m_context.last.operator_.reset();

    // run_callbacks();

    // If exit is generated that means the rest of the code is unrecheable
    if (m_context.current_phase == Phase::EXITING ||
        m_context.current_phase == Phase::PRUNING_FULL) {

      if (m_context.current_phase == Phase::PRUNING_FULL) {
        m_context.current_phase = Phase::EXECUTING;
      }

      break;
    }
  }
}

instruction_t ast_traverser::generate_condition(const ast::expr::expression& cond) {
  auto* r      = generate_expr(cond);
  auto last_op = m_context.last.operator_;
  if (!last_op) {
    m_context.cmp(r->value(), "1");
    return instruction_t::je;
  }

  return last_op->ins.value();
}

void ast_traverser::begin_scope(const ast::scope::block* stmts) {
  m_context.table.active_frame().create_scope(*stmts);
}

void ast_traverser::end_scope() {
  size_t ditched = m_context.table.active_frame().destroy_scope();

  if (ditched > 0) {
    m_context.move_rsp(ditched);
  }
}

std::optional<operand*> ast_traverser::call_function(const ast::term::term& t,
                                                     const ast::expr::call::arguments& args) {
  return call_function(t, args.types(), args);
}

std::optional<assembly::operand*> ast_traverser::call_function(
    const ir::function* fn,
    const ast::expr::call::arguments& args) {
  return fn->run(m_context, args);
}
std::optional<assembly::operand*> ast_traverser::call_function(
    const ast::term::term& t,
    const std::vector<ptr_type>& types,
    const ast::expr::call::arguments& args) {
  function::signature_t sig(t.format(), types);
  const function* func = m_context.table.get_function(sig);
  if (nullptr == func) {
    throw_error<_error_t::UNDECLARED_SYMBOL>(t);
  }
  if (!func->is_defined()) {
    throw_error<_error_t::UNDEFINED_FUNCTION>(t);
  }
  return call_function(func, args);
}

std::optional<std::tuple<std::string, std::string>> ast_traverser::get_label_interation_scope()
    const {
  for (const auto& scope : m_context.table.active_frame().scopes) {
    const auto* parent = scope.compound.get().get_parent();
    if (const auto* it = dynamic_cast<const iteration::while_*>(parent)) {
      return {
          {it->condition_label(), it->exit_label()}
      };
    }
    if (const auto* it = dynamic_cast<const iteration::for_*>(parent)) {
      return {
          {it->condition_label(), it->exit_label()}
      };
    }
  }

  return {};
}

template <typename Jump>
void ast_traverser::generate_continue_break(const Jump& node) {
  // Check if there is a visible iteration scope
  auto res = get_label_interation_scope();
  std::string label;

  if constexpr (std::is_same_v<ast::jump::continue_, Jump>) {

    if (!res.has_value()) {
      throw_error<error_t::INVALID_CONTINUE>(node);
    }
    const auto& [cond, exit] = res.value();
    label                    = cond;
  } else {
    if (!res.has_value()) {
      throw_error<error_t::INVALID_BREAK>(node);
    }
    const auto& [cond, exit] = res.value();
    label                    = exit;
  }

  m_context.jump(label);
  m_context.current_phase = Phase::PRUNING_FULL;
}

template void ast_traverser::generate_continue_break<ast::jump::continue_>(
    const ast::jump::continue_&);
template void ast_traverser::generate_continue_break<ast::jump::break_>(const ast::jump::break_&);

template <bool IsGlobal>
void ast_traverser::generate_variable_decl(const ast::decl::variable* vardecl) {
  if (!m_context.table.is_declarable<variable>(vardecl->ident)) {
    throw_error<error_t::ALREADY_DECLARED_SYMBOL>(vardecl->ident);
  }
  auto b = m_context.asmgen.begin_comment_block("init variable {}", vardecl->ident.value());
  const auto& ident = vardecl->ident;

  operand* reg_     = nullptr;
  if (auto* defined = vardecl->init) {
    reg_ = generate_expr(*defined);
  } else {
    reg_ = m_context.move_immediate(m_context.regs.get(registers::ACCUMULATOR), "0");
  }

  if constexpr (IsGlobal) {
    m_context.declare_global_variable(*vardecl, reg_);
  } else {
    m_context.declare_local_variable(*vardecl, reg_);
  }
}
void global_visitor::visit(const ast::decl::variable& vardecl) {
  gen->generate_variable_decl<true>(&vardecl);
}

void global_visitor::visit(const ast::decl::function& func) {
  REGISTER_TRACE("Generating function {}", func.ident.value());
  if (func.ident.value() == "main") {
    // Call main
    if (gen->m_context.table.is_entry_point_defined()) {
      throw_error<_error_t::ALREADY_DECLARED_SYMBOL>(func.ident);
    }
    gen->m_context.table.link_entry_point(&func);
    gen->m_context.current_phase = Phase::EXECUTING;
    gen->m_context.table.get_entry_point()->run(gen->m_context);
    if (gen->m_context.current_phase != Phase::EXITING) {
      gen->m_context.exit_successfully();
      gen->m_context.current_phase = Phase::EXITING;
    }
  } else {
    if (gen->m_context.table.is_declared<function>(func.ident)) {
      throw_error<_error_t::ALREADY_DECLARED_SYMBOL>(func.ident);
    }
    gen->m_context.table.declare_function(&func);
  }
}
expression_visitor::expression_visitor(ast_traverser* t, operand* o, intent_t l)
    : gen(t),
      in(o),
      out(o),
      intent(l) {}

#define gen_op(IN) value_or(generate<operand*>(in))
void expression_visitor::visit(const expr::call& expr_call) {
  out = gen->call_function(expr_call.ident, expr_call.args).gen_op(in);
}

void expression_visitor::visit(const expr::binary_operator& binop) {
  auto op = binop.operator_.type;
  out     = gen->call_function(binop.semantics.fn, {&binop.left, &binop.right})
            .value_or(generate<operand*>(in));
  gen->m_context.last.operator_.emplace(op);
}

void expression_visitor::visit(const expr::unary_operator& unary) {
  auto op    = unary.operator_.type;
  operand* r = gen->call_function(unary.semantics.fn, {&unary.expr}).gen_op(in);
  gen->m_context.move(out, r);
  gen->m_context.last.operator_.emplace(op);
}
void expression_visitor::visit(const ast ::expr ::float_lit& c) {
  gen->m_context.move_immediate(in, c.term.value());
};
void expression_visitor::visit(const ast ::expr ::int_lit& c) {
  gen->m_context.move_immediate(in, c.term.value());
}
void expression_visitor::visit(const ast ::expr ::char_lit& c) {
  const auto& pair = gen->m_context.reserve_constant(c.term.value());
  gen->m_context.move(in, pair);
};
void expression_visitor::visit(const ast ::expr ::string_lit& c) {
  const auto& pair = gen->m_context.reserve_constant(c.term.value());
  gen->m_context.move(in, pair);
};
void expression_visitor::visit(const ast ::expr ::false_lit&) {
  gen->m_context.move_immediate(in, "0");
}
void expression_visitor::visit(const ast ::expr ::true_lit&) {
  gen->m_context.move_immediate(in, "1");
}

void expression_visitor::visit(const expr::identifier& ident) {
  auto term       = ident.term;
  const auto* var = gen->m_context.table.get_variable(term);
  auto* addr      = var->address;
  switch (intent) {
    case intent_t::LOAD_VARIABLE_VALUE:
      {
        auto b = gen->m_context.asmgen.begin_comment_block("loading value of var {}", term.value());
        out    = gen->m_context.move(out, addr);
      }
      break;

    case intent_t::LOAD_VARIABLE_ADDRESS:
      {
        auto b = gen->m_context.asmgen.begin_comment_block("loading address of {}", term.value());
        out    = gen->m_context.lea(out, addr);
      }
      break;
    case intent_t::SAVE_VARIABLE_VALUE:
      {
        auto b = gen->m_context.asmgen.begin_comment_block("saving value of {}", term.value());
        out    = gen->m_context.move(addr, in);
      }
    case intent_t::MOVE_CONTENT:
    default:
      throw_error<error_t::GENERIC>(cmm::os::status::GENERIC_ERROR, "intent not allowed");
  }
}

statement_visitor::statement_visitor(ast_traverser* gen_)
    : expression_visitor(gen_,
                         gen_->m_context.regs.get(registers::ACCUMULATOR),
                         intent_t::LOAD_VARIABLE_VALUE),
      gen(gen_) {}

void statement_visitor::visit(const ast::scope::block& scope) {
  // Create scope
  gen->begin_scope(&scope);

  // Handle every element within scope
  gen->generate_statements(scope);

  // Discard scope's elements
  gen->end_scope();
}

void statement_visitor::visit(const decl::variable& vardecl) {
  gen->generate_variable_decl<false>(&vardecl);
}

void statement_visitor::visit(const decl::label& label_) {
  if (gen->m_context.table.is_declared<label>(label_.term)) {
    throw_error<error_t::ALREADY_DECLARED_SYMBOL>(label_.term);
  }

  gen->m_context.declare_label(label_);
  gen->m_context.label(label_.term.value());

  if (gen->m_context.current_phase == Phase::PRUNING_LABEL &&
      gen->m_context.last.pruning_label->value() == label_.term.value()) {
    gen->m_context.current_phase = Phase::EXECUTING;
  }
}

void statement_visitor::visit(const decl::function&) { UNREACHABLE("not implemented"); }

void statement_visitor::visit(const iteration::for_& for_) {
  if (for_.start != nullptr) {
    // Generate init statement
    gen->generate_statement(for_.start);
  }

  gen->m_context.asmgen.write_label(for_.condition_label());
  // Checking condition
  if (auto* cond = for_.condition) {
    auto jmp_if     = gen->generate_condition(*cond);
    auto jmp_if_not = jmp_if.inverse_jump.value();

    // If false exit while
    gen->m_context.jump(jmp_if_not, for_.exit_label());
  }

  // If true execute body and check cond again
  if (auto* body = for_.body) {
    gen->generate_statement(body);
  }

  if (auto* step = for_.step) {
    gen->generate_expr(*step);
  }
  gen->m_context.jump(for_.condition_label());
  gen->m_context.asmgen.write_label(for_.exit_label());
}

void statement_visitor::visit(const iteration::while_& while_) {
  gen->m_context.asmgen.write_label(while_.condition_label());

  // Checking condition
  auto jmp_if     = gen->generate_condition(while_.condition);
  auto jmp_if_not = jmp_if.inverse_jump.value();

  // If false exit while
  gen->m_context.jump(jmp_if_not, while_.exit_label());

  // If true execute body and check cond again
  if (auto* body = while_.body) {
    gen->generate_statement(body);
  }
  gen->m_context.jump(while_.condition_label());
  gen->m_context.asmgen.write_label(while_.exit_label());
}

void statement_visitor::visit(const selection::if_& if_) {
  // Checking condition
  auto block      = gen->m_context.asmgen.begin_comment_block("if condition");
  auto jmp_if     = gen->generate_condition(if_.condition);
  auto jmp_if_not = jmp_if.inverse_jump.value();
  block.end();

  cstring after_if_label = "after_if";
  gen->m_context.jump(jmp_if_not, after_if_label);
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
void statement_visitor::visit(const jump::break_& break_) {
  gen->generate_continue_break<jump::break_>(break_);
}
void statement_visitor::visit(const jump::continue_& continue_) {
  auto b = gen->m_context.asmgen.begin_comment_block("continue");
  gen->generate_continue_break<jump::continue_>(continue_);
  b.end();
}

void statement_visitor::visit(const jump::goto_& goto_) {
  gen->m_context.jump(goto_.term);
  if (!gen->m_context.table.is_declared<label>(goto_.term)) {
    gen->m_context.current_phase      = Phase::PRUNING_LABEL;
    gen->m_context.last.pruning_label = &goto_.term;
  }
}

void statement_visitor::visit(const jump::return_& ret) {
  if (gen->m_context.current_phase == Phase::PRUNING_LABEL) {
    return;
  }

  if (gen->m_context.table.is_global_scope()) {
    throw_error<error_t::RETURN_IN_GLOBAL>(ret);
  }

  operand* op = nullptr;
  if (ret.expr != nullptr) {
    op = gen->generate_expr(*ret.expr);
    REGISTER_DEBUG("{}", op->value());
    // We type_storagereturn value in proper register
    // but happens to be the same
    /* gen->m_context.source.move(registers::accumulator.get(),
     * registers::accumulator); */
  } else {
    op = gen->m_context.zero(gen->m_context.regs.get(registers::ACCUMULATOR));
  }

  if (!gen->m_context.table.in_main()) {
    gen->m_context.current_phase = Phase::PRUNING_FULL;
    gen->m_context.ret();
  } else {
    gen->m_context.exit(op);
    gen->m_context.current_phase = Phase::EXITING;
  }
}

} // namespace cmm::ir
