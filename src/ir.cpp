#include "ir.hpp"

#include <format>
#include <ranges>
#include <utility>
#include <vector>

#include "asm.hpp"
#include "ast.hpp"
#include "common.hpp"
#include "expr.h"
#include "lang.hpp"
#include "types.hpp"

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
    auto* aux = regs.get(register_t::SCRATCH_1);
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
  move(regs.get(register_t::ACCUMULATOR), r);
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
  move_immediate(regs.get(register_t::ACCUMULATOR), num_syscall);
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
  if (!ast->is_entry_point_defined()) {
    // throw_error<compilation_error_t::MISSING_ENTRY_POINT>();
  }

  REGISTER_TRACE("Code generation finished");
  return asmgen.end();
}

std::vector<operand*> compilation_unit::load_arguments(const ast::decl::function* fn,
                                                       const ast::expr::arguments& args) {
  auto registers = regs.parameters();
  return std::views::zip(fn->params, args) |
         std::views::transform([this, &registers, &fn](const auto& pair) {
           const auto& [param, arg] = pair;
           auto* reg = runner.generate_expr(*arg, param->specs.type, registers.next());
           return fn->body->declare_parameter(param, reg);
         }) |
         std::ranges::to<std::vector>();
}
std::optional<assembly::operand*> compilation_unit::call_function(decl::signature sig,
                                                                  const ast::expr::arguments& args,
                                                                  bool inlined_) {
  if (asmgen::exists_snippet(sig.name.value())) {
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
  auto ps     = load_arguments(fn, args);
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
      return v.regs.parameter_at(1);
    default:
      NOT_IMPLEMENTED
  }
}
} // namespace

std::vector<long> compilation_unit::progressive_prefix_match(
    const std::vector<ptype>& argument_types,
    const std::vector<std::pair<long, std::vector<ptype>>>& possible_fns) const {
  return argument_types | std::views::enumerate |
         std::views::transform([this, &possible_fns](const auto& pair) {
           auto i                 = std::get<0>(pair);
           ptype castable_type = std::get<1>(pair);

           auto r                 = conversions::get_convertible_types(castable_type) |
                    std::views::transform([this, i, &possible_fns](ptype casted_type) {
                      auto p =
                          possible_fns | std::views::filter([this, i, casted_type](const auto& fn) {
                            const auto& [j, types] = fn;
                            return types.at(i)->format() == casted_type->format();
                          }) |
                          std::views::transform([](const auto& pair) { return std::get<0>(pair); });
                      return p;
                    }) |
                    std::views::join | std::ranges::to<std::vector>();
           return r;
         }) |
         std::views::join | std::ranges::to<std::vector>();
}
operator_builtin_data compilation_unit::get_operator_implementation(
    const operator_& op,
    const std::vector<ptype>& types) const {
  std::vector<operator_builtin_data> builtins({get_builtin_operator(op, types).value()});
  if (builtins.size() == 1) {
    return builtins.front();
  }

  auto args = builtins | std::views::enumerate | std::views::transform([](const auto& pair) {
                const auto& [i, types] = pair;
                return std::make_pair(i, std::vector<ptype>{types.arg1, types.arg2});
              }) |
              std::ranges::to<std::vector>();
  auto result = progressive_prefix_match(types, args) | std::ranges::to<std::vector>();
  if (result.empty()) {
    throw_error<compilation_error_t::UNDECLARED_SYMBOL>(op);
  }
  if (result.size() > 1) {
    REGISTER_WARN("Matched operator {} more than one", op);
  }
  return builtins.at(result.front());
}

operand* compilation_unit::builtin_operator(const operator_& op,
                                            const std::vector<ptype>& types,
                                            const std::vector<operand*>& ops) {
  REGISTER_INFO("Calling builtin operator {}", op);
  operand* res   = nullptr;
  auto operators = get_operator_implementation(op, types);
  for (const auto& exec_unit : operators.ins) {
    instruction_t ins = exec_unit.ins;
    instruction_data data(ins);
    operand* l = ops.at(0);
    operand* r = ops.at(1);

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
        res = regs.get(register_t::ACCUMULATOR);
        break;
      case instruction_result_reg::NONE:
        break;
    }
  }
  return res;
}
static_assert(std::formattable<operand*, char>);

#include "ir.inl"
} // namespace cmm::ir
