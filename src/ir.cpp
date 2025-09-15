#include "ir.hpp"

#include <format>
#include <ranges>
#include <vector>

#include "asm.hpp"
#include "ast.hpp"
#include "common.hpp"
#include "expr.h"
#include "lang.hpp"
#include "traverser.hpp"
#include "types.hpp"

namespace cmm {
class source_code;
} // namespace cmm

namespace cmm::ir {

using namespace cmm::assembly;
using namespace cmm::ast;

compilation_unit::compilation_unit(ast::translation_unit* t_ast, source_code* t_code)
    : ast(t_ast),
      code(t_code),
      runner(*this) {
  ast->cunit = this;
}

source_code* compilation_unit::compile() {
  start();
  runner.generate_program(*ast);
  code->set_compiled(end());
  return code;
}

void compilation_unit::call(std::string_view func) {
  asmgen.write_instruction(instruction_t::call, func);
}

operand* compilation_unit::move(operand* to, operand* from) {
  if (to == from) {
    return to;
  }

  if (to->type() == operand::type_t::MEMORY && from->type() == operand::type_t::MEMORY) {
    auto* aux = regs.get(register_t::SCRATCH_1);
    asmgen.write_instruction(instruction_t::mov, aux->value(), from->value());
    asmgen.write_instruction(instruction_t::mov, to->value(), aux->value());
    aux->release();
  } else {
    asmgen.write_instruction(instruction_t::mov, to->value(), from->value());
  }

  return to;
}
void compilation_unit::move_rsp(int64_t t_offset) { assert(false); }

operand* compilation_unit::return_reg(operand* r) {
  move(regs.get(register_t::ACCUMULATOR), r);
  return r;
}

operand* compilation_unit::move_immediate(operand* r, std::string_view lit) {
  asmgen.write_instruction(instruction_t::mov, r->value(), lit);
  return r;
}

operand* compilation_unit::lea(operand* to, operand* from) {
  asmgen.write_instruction(instruction_t::lea, to->value(), from->value());
  to->hold(from->variable());
  return to;
}

operand* compilation_unit::zero(operand* reg) {
  asmgen.write_instruction(instruction_t::xor_, reg->value(), reg->value());
  return reg;
}

void compilation_unit::reserve_memory(std::string_view name,
                                      std::string_view kw,
                                      std::string_view times) {
  asmgen.add_bss(name, kw, times);
}

void compilation_unit::reserve_static_var(std::string_view name) {
  reserve_memory(name, "resb", "8");
}

assembly::label_literal* compilation_unit::reserve_constant(std::string_view value) {
  std::string name = std::format("str_{}", counters.literals++);
  asmgen.add_data(name, value);
  return operand_factory::create<assembly::label_literal>(name);
}

void compilation_unit::jump(std::string_view label) {
  asmgen.write_instruction(instruction_t::jmp, label);
}

void compilation_unit::jump(const instruction_t& ins, std::string_view a) {
  asmgen.write_instruction(ins, a);
}

void compilation_unit::cmp(std::string_view a, std::string_view b) {
  asmgen.write_instruction(instruction_t::cmp, a, b);
}

void compilation_unit::ret() { asmgen.write_instruction(instruction_t::ret); }

void compilation_unit::exit(operand* op) {
  move(regs.parameter_at(0), op);
  jump("exit");
}

void compilation_unit::exit_successfully() {
  auto* arg = move_immediate(regs.parameter_at(0), "0");
  this->exit(arg);
}

void compilation_unit::syscall() { asmgen.write_instruction(instruction_t::syscall); }

void compilation_unit::syscall(std::string_view num_syscall) {
  move_immediate(regs.get(register_t::ACCUMULATOR), num_syscall);
  syscall();
}

void compilation_unit::label(std::string_view l) { asmgen.write_label(l); }

void compilation_unit::comment(std::string_view l) { asmgen.write_comment(l); }

void compilation_unit::start() {
  current_phase = Phase::GENERATING;
  asmgen.start();
}

void compilation_unit::push(operand* r) {
  asmgen.write_instruction(instruction_t::push, r->value());
}

void compilation_unit::pop(operand* r) {
  asmgen.write_instruction(instruction_t::pop, r->value());
  ast->active_frame()->local_stack.pop();
}

std::string compilation_unit::end() {
  REGISTER_TRACE("Code generation finished");
  return asmgen.end();
}

std::optional<assembly::operand*> compilation_unit::call_function(
    const identifier& id,
    const ast::expr::arguments& args) {
  if (asmgen::exists_snippet(id.value())) {
    asmgen.include_snippet(id);
    call(id);
    return {};
  }

  const decl::function* fn = ast->get_callable<decl::function>(id, args);
  if (nullptr == fn) {
    THROW(UNDECLARED_SYMBOL, id);
  }
  if (fn->body == nullptr) {
    THROW(UNDEFINED_FUNCTION, id);
  }

  load_arguments(fn->parameters(), args);
  call(fn->ident);

  if (fn->specs.type.value() != MANAGER.make(types::core_t ::void_t, {})) {
    return regs.get(register_t::ACCUMULATOR);
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
      return v.regs.get(register_t::ACCUMULATOR);
    case arg_t::AUX1:
      return v.regs.parameter_at(0);
    case arg_t::AUX2:
    default:
      return v.regs.parameter_at(1);
  }
}
} // namespace

operand* compilation_unit::call_builtin_operator(const operator_& op,
                                                 const ast::expr::arguments& args) {
  REGISTER_INFO("Calling builtin operator {}", op);
  operand* res = nullptr;
  auto* impls  = ast->get_callable<builtin_callable>(op, args);
  auto params  = regs.parameters();
  auto ops     = std::views::zip_transform(
                 [this, &params](const parameter& t_param, expr::expression* t_expr) {
                   return runner.generate_expr(
                       *translation_unit::bind_expression(t_expr, t_param.type), params.next());
                 },
                 impls->params,
                 args.data()) |
             std ::ranges ::to<std ::vector>();

  operand* l;
  operand* r;
  for (const auto& exec_unit : impls->ins) {
    instruction_t ins = exec_unit.ins;
    instruction_data data(ins);
    l = ops.at(0);
    if (ops.size() > 1) {
      r = ops.at(1);
    }
    if (data.n_params == 0) {
      instruction(ins);
    } else if (data.n_params == 1) {
      l = set_operand(*this, exec_unit.arg1, l, r);
      instruction(ins, l);
    } else if (data.n_params == 2) {
      l = set_operand(*this, exec_unit.arg1, l, r);
      r = set_operand(*this, exec_unit.arg2, l, r);
      instruction(ins, l, r);
    } else {
    }

    switch (data.where) {
      case instruction_result_reg::LEFT:
        res = l;
        break;
      case instruction_result_reg::RIGHT:
        res = r;
        break;
      case instruction_result_reg::ACCUMULATOR:
        res = regs.get(register_t::ACCUMULATOR);
        break;
      case instruction_result_reg::NONE:
        break;
    }
  }

  return res;
}

namespace {
intent_t mode_to_intent(binding_mode_t t_mode) {
  using enum intent_t;
  using enum binding_mode_t;
  switch (t_mode) {
    case DIRECT:
      return LOAD_VARIABLE_ADDRESS;
    case COPY:
      return LOAD_VARIABLE_VALUE;
  }
  return MOVE_CONTENT;
}
} // namespace
void compilation_unit::load_arguments(const parameters& parameters, const expr::arguments& args) {
  auto param_regs = regs.parameters();
  for (const auto& [param, arg] : std::views::zip(parameters, args)) {
    types::type_id param_type     = param.type;
    binding_mode_t mode           = ast->bind_value(arg->value_category(), param_type);

    ast::expr::expression* n_expr = param.init == nullptr ? arg : param.init;
    if (n_expr == nullptr) {
      THROW(BAD_FUNCTION_CALL, arg);
    }

    intent_t intent = mode_to_intent(mode);
    runner.generate_expr(*n_expr, param_regs.next(), intent);
  }
}

} // namespace cmm::ir
