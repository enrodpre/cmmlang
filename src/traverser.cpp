#include "asm.hpp"
#include "ast.hpp"
#include "common.hpp"
#include "expr.h"
#include "ir.hpp"
#include <libassert/assert.hpp>
#include <traverser.hpp>

#include "lang.hpp"
#include "types.hpp"

namespace cmm::ir {

using namespace ast;
using namespace assembly;
using intents::intent_t;

translation_unit& ast_completer::complete(translation_unit& t) {
  conversions_visitor visitor;
  for (auto& decl : t.stmts) {
    decl->accept(visitor);
  }
  return t;
}

void ast_completer::conversions_visitor::visit(ast::expr::binary_operator& bin) {
  REGISTER_INFO("visited{}", bin);
  if (bin.semantics.original_type->category == type_category_t::bool_t) {
    bin.left = *bool_wrap_if(&bin.left);
  }
  if (bin.semantics.original_type->category == type_category_t::bool_t) {
    bin.right = *bool_wrap_if(&bin.right);
  }
}
void ast_completer::conversions_visitor::visit(ast::expr::unary_operator& un) {
  if (un.semantics.original_type->category == type_category_t::bool_t) {
    un.expr = *bool_wrap_if(&un.expr);
  }
}
void ast_completer::conversions_visitor::visit(ast::iteration::for_& f) {
  f.condition = bool_wrap_if(f.condition);
}
void ast_completer::conversions_visitor::visit(ast::iteration::while_& w) {
  w.condition = *bool_wrap_if(&w.condition);
}
void ast_completer::conversions_visitor::visit(ast::selection::if_& i) {
  i.condition = *bool_wrap_if(&i.condition);
}
expr::expression* ast_completer::conversions_visitor::bool_wrap_if(expr::expression* e) {
  if (e != nullptr && e->type().category != type_category_t::bool_t) {
    return allocator.emplace<expr::implicit_type_conversion>(*e, conversions::TO_BOOL);
  }
  return e;
}

ast_traverser::ast_traverser(compilation_unit& cunit)
    : m_context(cunit),
      ast() {}

global_visitor::global_visitor(ast_traverser* gen_)
    : gen(gen_) {}

void ast_traverser::generate_program(translation_unit& p) {
  ast_completer::complete(p);
  ast = &p;
  global_visitor visitor{this};
  // REGISTER_INFO("Program:\n{}", p.repr(0));
  for (auto* decl : p.stmts) {
    decl->accept(visitor);
  }
}

operand* ast_traverser::generate_expr(expr::expression& expr,
                                      intents::intent_t intent_,
                                      operand* reg_) {

  auto visitor = expression_visitor{this, reg_, intent_};
  expr.accept(visitor);
  return visitor.out;
}

operand* ast_traverser::generate_expr(expr::expression& expr, ir::intents::intent_t intent) {
  return generate_expr(expr, intent, m_context.regs.get(registers::ACCUMULATOR));
};

operand* ast_traverser::generate_expr(expr::expression& expr) {
  return generate_expr(
      expr, intent_t::LOAD_VARIABLE_VALUE, m_context.regs.get(registers::ACCUMULATOR));
}

assembly::operand* ast_traverser::generate_expr(expr::expression& expr,
                                                cr_type t,
                                                assembly::operand* o) {
  return generate_expr(expr,
                       is_indirect_v::operator()(t) ? intents::intent_t::LOAD_VARIABLE_ADDRESS
                                                    : intents::intent_t::LOAD_VARIABLE_VALUE,
                       o);
}
void ast_traverser::generate_statement(statement* stmt) {
  // m_context.src_location = stmt->location();
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
        m_context.current_phase = Phase::EXECUTING;
      }

      break;
    }
  }
}

instruction_t ast_traverser::generate_condition(expr::expression& cond) {
  auto* r      = generate_expr(cond);
  auto last_op = m_context.last.operator_;
  if (!last_op) {
    m_context.cmp(r->value(), "1");
    return instruction_t::je;
  }

  return {}; // last_op->ins.value();
}

void ast_traverser::begin_scope(decl::block& stmts) { ast->active_frame()->create_scope(stmts); }

void ast_traverser::end_scope() {
  size_t ditched = ast->active_frame()->destroy_scope();

  if (ditched > 0) {
    NOT_IMPLEMENTED;
    // m_context.move_rsp(ditched);
  }
}

//
// std::optional<assembly::operand*> ast_traverser::call_function(const identifier& t,
//                                                                const std::vector<ptr_type>&
//                                                                types, const expr::arguments&
//                                                                args) {
//   decl::signature sig(t.value(), types);
//   const decl::function* func = ast->get_function(sig);
//   return call_function(func, args);
// }

namespace {
  [[nodiscard]] std::string condition_label() { return std::format("cond_loop"); }
  [[nodiscard]] std::string exit_label() { return std::format("exit_loop"); }
} // namespace
std::optional<std::tuple<std::string, std::string>> ast_traverser::get_label_interation_scope()
    const {
  for (const auto& scope : ast->active_frame()->local_scopes) {
    const auto* parent = scope->get_parent();
    if (const auto* it = dynamic_cast<const iteration::while_*>(parent)) {
      return {
          {condition_label(), exit_label()}
      };
    }
    if (const auto* it = dynamic_cast<const iteration::for_*>(parent)) {
      return {
          {condition_label(), exit_label()}
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

  if constexpr (std::is_same_v<jump::continue_, Jump>) {

    if (!res.has_value()) {
      throw_error<compilation_error_t::INVALID_CONTINUE>(node);
    }
    const auto& [cond, exit] = res.value();
    label                    = cond;
  } else {
    if (!res.has_value()) {
      throw_error<compilation_error_t::INVALID_BREAK>(node);
    }
    const auto& [cond, exit] = res.value();
    label                    = exit;
  }

  m_context.jump(label);
  m_context.current_phase = Phase::PRUNING_FULL;
}

template void ast_traverser::generate_continue_break<jump::continue_>(const jump::continue_&);
template void ast_traverser::generate_continue_break<jump::break_>(const jump::break_&);

template <bool IsGlobal>
void ast_traverser::generate_variable_decl(decl::variable* vardecl) {
  if (!ast->is_declarable<ast::decl::variable>(vardecl->ident)) {
    throw_error<compilation_error_t::ALREADY_DECLARED_SYMBOL>(vardecl->ident);
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
    ast->declare_variable(vardecl);
    m_context.move(vardecl->address, reg_);
  } else {
    ast->declare_variable(vardecl);
    m_context.push(reg_);
  }
}
void global_visitor::visit(decl::variable& vardecl) { gen->generate_variable_decl<true>(&vardecl); }

void global_visitor::visit(decl::function& func) {
  REGISTER_TRACE("Generating function {}", func.ident.value());
  if (func.ident.value() == "main") {
    // Call main
    if (gen->ast->is_entry_point_defined()) {
      throw_error<compilation_error_t::ALREADY_DECLARED_SYMBOL>(func.ident);
    }
    gen->ast->link_entry_point(&func);
    gen->m_context.current_phase = Phase::EXECUTING;
    builtin_signature_data sig   = builtin_signature_t::MAIN;
    gen->m_context.call_function(sig.signature(), {}, true);
    if (gen->m_context.current_phase != Phase::EXITING) {
      gen->m_context.exit_successfully();
      gen->m_context.current_phase = Phase::EXITING;
    }
  } else {
    gen->ast->declare_function(&func);
  }
}
expression_visitor::expression_visitor(ast_traverser* t, operand* o, intent_t l)
    : gen(t),
      in(o),
      out(o),
      intent(l) {}

#define gen_op(IN) value_or(generate<operand*>(in))
void expression_visitor::visit(expr::call& expr_call) {
  decl::signature sig{expr_call.ident, expr_call.types()};
  out = gen->m_context.call_function(sig, expr_call.args).gen_op(in);
}

void expression_visitor::visit(ast::expr::implicit_type_conversion&) { NOT_IMPLEMENTED }
void expression_visitor::visit(expr::binary_operator& binop) {
  auto op        = binop.operator_.value();
  auto params    = gen->m_context.regs.parameters();
  auto* left_op  = gen->generate_expr(binop.left, intent_t::LOAD_VARIABLE_VALUE, params.next());
  auto* right_op = gen->generate_expr(binop.right, intent_t::LOAD_VARIABLE_VALUE, params.next());

  out            = gen->m_context.builtin_operator(binop, left_op, right_op);
  gen->m_context.last.operator_.emplace(op);
  // Implicit call to ~params()
}

void expression_visitor::visit(expr::unary_operator& unary) {
  auto op       = unary.operator_.value();
  auto params   = gen->m_context.regs.parameters();
  auto* expr_op = gen->generate_expr(unary.expr, intent_t::LOAD_VARIABLE_VALUE, params.next());
  out           = gen->m_context.builtin_operator(unary, expr_op);
  gen->m_context.last.operator_.emplace(op);
  // Implicit call to ~params()
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
  const auto* var = gen->ast->get_variable(ident);
  auto* addr      = var->address;
  ASSERT(addr != nullptr);
  switch (intent) {
    case intent_t::LOAD_VARIABLE_VALUE:
      {
        auto b =
            gen->m_context.asmgen.begin_comment_block("loading value of var {}", ident.value());
        out = gen->m_context.move(out, addr);
      }
      break;

    case intent_t::LOAD_VARIABLE_ADDRESS:
      {
        auto b = gen->m_context.asmgen.begin_comment_block("loading address of {}", ident.value());
        out    = gen->m_context.lea(out, addr);
      }
      break;
    case intent_t::SAVE_VARIABLE_VALUE:
      {
        auto b = gen->m_context.asmgen.begin_comment_block("saving value of {}", ident.value());
        out    = gen->m_context.move(addr, in);
      }
    case intent_t::MOVE_CONTENT:
    default:
      throw_error<compilation_error_t::GENERIC>("intent not allowed");
  }
}

statement_visitor::statement_visitor(ast_traverser* gen_)
    : expression_visitor(gen_,
                         gen_->m_context.regs.get(registers::ACCUMULATOR),
                         intent_t::LOAD_VARIABLE_VALUE),
      gen(gen_) {}

void statement_visitor::visit(ast::decl::block& scope) {
  // Create scope
  gen->begin_scope(scope);

  // Handle every element within scope
  gen->generate_statements(scope);

  // Discard scope's elements
  gen->end_scope();
}

void statement_visitor::visit(decl::variable& vardecl) {
  gen->generate_variable_decl<false>(&vardecl);
}

void statement_visitor::visit(decl::label& label_) {
  if (gen->ast->is_declarable<decl::label>(label_.ident)) {
    throw_error<compilation_error_t::ALREADY_DECLARED_SYMBOL>(label_);
  }

  gen->ast->active_frame()->declare_label(&label_);
  gen->m_context.label(label_.ident.value());

  if (gen->m_context.current_phase == Phase::PRUNING_LABEL &&
      gen->m_context.last.pruning_label->value() == label_.ident.value()) {
    gen->m_context.current_phase = Phase::EXECUTING;
  }
}

void statement_visitor::visit(decl::function&) { UNREACHABLE("not implemented"); }

void statement_visitor::visit(iteration::for_& for_) {
  if (for_.start != nullptr) {
    // Generate init statement
    gen->generate_statement(for_.start);
  }

  gen->m_context.asmgen.write_label(condition_label());
  // Checking condition
  if (auto* cond = for_.condition) {
    // TODO
    // auto jmp_if     = gen->generate_condition(*cond);
    // auto jmp_if_not = jmp_if.inverse_jump.value();
    //
    // // If false exit while
    // gen->m_context.jump(jmp_if_not, exit_label());
  }

  // If true execute body and check cond again
  if (auto* body = for_.body) {
    gen->generate_statement(body);
  }

  if (auto* step = for_.step) {
    gen->generate_expr(*step);
  }
  gen->m_context.jump(condition_label());
  gen->m_context.asmgen.write_label(exit_label());
}

void statement_visitor::visit(iteration::while_& while_) {
  gen->m_context.asmgen.write_label(condition_label());

  // Checking condition
  // //TODO
  // auto jmp_if     = gen->generate_condition(while_.condition);
  // auto jmp_if_not = jmp_if.inverse_jump.value();
  //
  // // If false exit while
  // gen->m_context.jump(jmp_if_not, exit_label());

  // If true execute body and check cond again
  if (auto* body = while_.body) {
    gen->generate_statement(body);
  }
  gen->m_context.jump(condition_label());
  gen->m_context.asmgen.write_label(exit_label());
}

void statement_visitor::visit(selection::if_& if_) {
  // Checking condition
  auto block = gen->m_context.asmgen.begin_comment_block("if condition");
  // TODO
  // auto jmp_if     = gen->generate_condition(if_.condition);
  // auto jmp_if_not = jmp_if.inverse_jump.value();
  block.end();

  cstring after_if_label = "after_if";
  // gen->m_context.jump(jmp_if_not, after_if_label);
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
    throw_error<compilation_error_t::RETURN_IN_GLOBAL>(ret);
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

  if (!gen->ast->in_main()) {
    gen->m_context.current_phase = Phase::PRUNING_FULL;
    gen->m_context.ret();
  } else {
    gen->m_context.exit(op);
    gen->m_context.current_phase = Phase::EXITING;
  }
}

} // namespace cmm::ir
