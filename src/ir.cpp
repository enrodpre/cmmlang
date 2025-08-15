#include "ir.hpp"
#include "asm.hpp"
#include "ast.hpp"
#include "common.hpp"
#include "expr.h"
#include "lang.hpp"
#include "types.hpp"
#include <bits/ranges_algo.h>
#include <format>
#include <libassert/assert.hpp>
#include <magic_enum/magic_enum.hpp>
namespace cmm::ir {

using namespace cmm::assembly;
using namespace cmm::ast;

compilation_unit::compilation_unit()
    : runner(*this),
      ast() {}

std::string compilation_unit::compile(ast::translation_unit& p, const source_code* src) {
  source = src;
  ast    = &p;
  p.set_context(this);
  start();
  runner.generate_program(p);
  return end();
}

void compilation_unit::call(cstring func) { asmgen.write_instruction(instruction_t::call, func); }

operand* compilation_unit::move(operand* to, operand* from) {
  if (to == from) {
    return to;
  }

  if (to->type() == operand::type_t::MEMORY && from->type() == operand::type_t::MEMORY) {
    auto* aux = regs.get(registers::SCRATCH_1);
    asmgen.write_instruction(instruction_t::mov, aux->value(), from->value());
    asmgen.write_instruction(instruction_t::mov, to->value(), aux->value());
    aux->release();
  } else {
    asmgen.write_instruction(instruction_t::mov, to->value(), from->value());
  }

  if (auto cont = from->content(); cont) {
    to->hold_value(from->variable());
  }
  return to;
}

operand* compilation_unit::return_reg(operand* r) {
  move(regs.get(registers::ACCUMULATOR), r);
  return r;
}

operand* compilation_unit::move_immediate(operand* r, cstring lit) {
  asmgen.write_instruction(instruction_t::mov, r->value(), lit);
  return r;
}

operand* compilation_unit::lea(operand* to, operand* from) {
  asmgen.write_instruction(instruction_t::lea, to->value(), from->value());
  to->hold_address(from->variable());
  return to;
}

operand* compilation_unit::zero(operand* reg) {
  asmgen.write_instruction(instruction_t::xor_, reg->value(), reg->value());
  return reg;
}

void compilation_unit::reserve_memory(cstring name, cstring kw, cstring times) {
  asmgen.add_bss(name, kw, times);
}

void compilation_unit::reserve_static_var(cstring name) { reserve_memory(name, "resb", "8"); }

assembly::label_literal* compilation_unit::reserve_constant(cstring value) {
  std::string name = std::format("str_{}", counters.literals++);
  asmgen.add_data(name, value);
  return operand_factory::create<assembly::label_literal>(name);
}

void compilation_unit::jump(cstring label) { asmgen.write_instruction(instruction_t::jmp, label); }
void compilation_unit::jump(const instruction_t& ins, cstring a) {
  asmgen.write_instruction(ins, a);
}

void compilation_unit::cmp(cstring a, cstring b) {
  asmgen.write_instruction(instruction_t::cmp, a, b);
}

void compilation_unit::ret() { asmgen.write_instruction(instruction_t::ret); }
void compilation_unit::exit(operand* op) {
  move(regs.parameter_at(0), op);
  jump("exit");
}

void compilation_unit::exit_successfully() {
  auto* arg = move_immediate(regs.parameter_at(0), "0");
  exit(arg);
}

void compilation_unit::syscall() { asmgen.write_instruction(instruction_t::syscall); }
void compilation_unit::syscall(cstring num_syscall) {
  move_immediate(regs.get(registers::ACCUMULATOR), num_syscall);
  syscall();
}
void compilation_unit::label(cstring l) { asmgen.write_label(l); }
void compilation_unit::comment(cstring l) { asmgen.write_comment(l); }
void compilation_unit::start() {
  current_phase = Phase::GLOBAL;
  asmgen.start();
}
void compilation_unit::push(operand* r) {
  asmgen.write_instruction(instruction_t::push, r->value());
  ast->active_frame()->local_stack.push();
}
void compilation_unit::pop(operand* r) {
  asmgen.write_instruction(instruction_t::pop, r->value());
  ast->active_frame()->local_stack.pop();
}
std::string compilation_unit::end() {
  // if (!table.is_entry_point_defined()) {
  //   throw_error<_error_t::MISSING_ENTRY_POINT>();
  // }

  REGISTER_TRACE("Code generation finished");
  return asmgen.end();
}

std::optional<assembly::operand*> compilation_unit::call_function(decl::signature sig,
                                                                  const ast::expr::arguments& args,
                                                                  bool inlined_) {
  if (asmgen.exists_snippet(sig.name.value())) {
    asmgen.register_snippet(sig.name.value());
    call(sig.name.value());
    return {};
  }

  auto* fn = ast->get_function(sig);
  if (nullptr == fn) {
    throw_error<compilation_error_t::UNDECLARED_SYMBOL>(sig.name);
  }
  if (fn->body == nullptr) {
    throw_error<compilation_error_t::UNDEFINED_FUNCTION>(sig.name);
  }
  ast->create_frame(fn->body);
  auto* sc    = ast->active_scope();
  auto params = regs.parameters();
  auto ps     = std::views::zip(fn->params, args) |
            std::views::transform([this, &params, &fn](const auto& pair) {
              const auto& [param, arg] = pair;
              auto* reg = runner.generate_expr(*arg, param->specs.type, params.next());
              return fn->body->declare_parameter(param, reg);
            }) |
            std::ranges::to<std::vector>();

  if (!inlined_) {
    ast->active_frame()->local_stack.push();
    call(sig.name.string());
    asmgen.create_delay();
  }

  runner.generate_statements(*fn->body);

  if (!inlined_) {
    auto function_code = asmgen.dump_delayed();
    asmgen.register_labeled_code_block(fn->ident.string(), std::move(function_code));
  }

  size_t ditched_vars = ast->pop_frame();

  if (!ast->is_global_scope()) {
    ast->active_frame()->local_stack.pop(ditched_vars + 1);
  }

  if (&fn->specs.type.value() != VOID_T) {
    return regs.get(registers::ACCUMULATOR);
  }
  return {};
}
namespace {
  constexpr operand* set_operand(compilation_unit& v, arg_t side, operand* l, operand* r) {
    switch (side) {
      case arg_t::NONE:
        return nullptr;
      case arg_t::LEFT:
        return l;
      case arg_t::RIGHT:
        return r;
      case arg_t::ACC:
        return v.regs.get(registers::ACCUMULATOR);
      case arg_t::AUX1:
        return v.regs.parameter_at(0);
      case arg_t::AUX2:
        return v.regs.parameter_at(1);
      default:
        NOT_IMPLEMENTED
    }
  }
} // namespace
//
operand* compilation_unit::builtin_operator(expr::binary_operator& bin, operand* l, operand* r) {
  REGISTER_INFO("Calling builtin operator {}", bin.operator_);
  operand* res = nullptr;

  auto ops     = get_builtin_operator(bin.operator_.value()) | std::ranges::to<std::vector>();
  if (ops.size() < 1) {
    throw_error<compilation_error_t::UNDECLARED_SYMBOL>(bin.operator_);
  }

  for (const auto& ins : ops) {
    instruction_data data(ins.first);

    if (data.n_params == 0) {
      instruction(ins.first);
    } else if (data.n_params == 1) {
      l = set_operand(*this, ins.second.first, l, r);
      instruction(ins.first, l);
    } else if (data.n_params == 2) {
      l = set_operand(*this, ins.second.first, l, r);
      r = set_operand(*this, ins.second.second, l, r);
      instruction(ins.first, l, r);
    } else {
      NOT_IMPLEMENTED
    }

    switch (data.where) {
      case instruction_result_reg::LEFT:
        res = l;
        break;
      case instruction_result_reg::RIGHT:
        res = r;
        break;
      case instruction_result_reg::ACCUMULATOR:
        res = regs.get(registers::ACCUMULATOR);
        break;
      case instruction_result_reg::NONE:
        break;
    }
  }
  return res;
}
operand* compilation_unit::builtin_operator(expr::unary_operator& unary, operand* e) {
  REGISTER_INFO("Calling builtin operator {}", unary.operator_);
  operand* res = nullptr;
  auto ops     = get_builtin_operator(unary.operator_.value()) | std::ranges::to<std::vector>();
  if (ops.size() < 1) {
    throw_error<compilation_error_t::UNDECLARED_SYMBOL>(unary.operator_);
  }
  for (const auto& ins : ops) {

    instruction_data data(ins.first);
    if (data.n_params == 0) {
      instruction(ins.first);
    } else if (data.n_params == 1) {
      instruction(ins.first, e);
    } else {
      NOT_IMPLEMENTED
    }
    switch (data.where) {
      case instruction_result_reg::LEFT:
        res = e;
        break;
      case instruction_result_reg::ACCUMULATOR:
        res = regs.get(registers::ACCUMULATOR);
        break;
      case instruction_result_reg::RIGHT:
      case instruction_result_reg::NONE:
        break;
    }
  }
  return res;
}
static_assert(std::formattable<operand*, char>);

} // namespace cmm::ir
