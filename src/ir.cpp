#include "ir.hpp"

#include <algorithm>
#include <format>
#include <ranges>
#include <type_traits>
#include <utility>
#include <vector>

#include "asm.hpp"
#include "ast.hpp"
#include "common.hpp"
#include "expr.h"
#include "lang.hpp"
#include "semantic.hpp"
#include "types.hpp"

namespace cmm::ir {

using namespace cmm::assembly;
using namespace cmm::ast;

compilation_unit::compilation_unit()
    : ast(),
      runner(*this) {}

std::string compilation_unit::compile(ast::translation_unit& p, const source_code* src) {
  source = src;
  ast    = &p;
  p.set_context(this);
  start();
  runner.generate_program(p);
  return end();
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

  if (auto cont = from->content(); cont) {
    to->hold_value(from->variable());
  }
  return to;
}

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
  to->hold_address(from->variable());
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
  exit(arg);
}

void compilation_unit::syscall() { asmgen.write_instruction(instruction_t::syscall); }

void compilation_unit::syscall(std::string_view num_syscall) {
  move_immediate(regs.get(register_t::ACCUMULATOR), num_syscall);
  syscall();
}

void compilation_unit::label(std::string_view l) { asmgen.write_label(l); }

void compilation_unit::comment(std::string_view l) { asmgen.write_comment(l); }

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
    // THROW(MISSING_ENTRY_POINT, );
  }

  REGISTER_TRACE("Code generation finished");
  return asmgen.end();
}

template <typename T, typename Id>
const T* compilation_unit::get_callable(Id id, const expr::arguments& argument_expressions) const {

  auto argument_types = argument_expressions | TRANSFORM(to_type) | TO_VEC;

  std::vector<const T*> overloads;
  std::vector<types::type_id> parameter_types;
  if constexpr (std::is_same_v<T, builtin_operator_data>) {
    overloads = get_builtin_operator(id.value());
  } else if constexpr (std::is_same_v<T, decl::function>) {
    overloads = ast->m_functions.get_by_name(id);
  } else {
    static_assert(false);
  }
  auto possible_exact_match =
      overloads | std::views::filter([argument_types](const T* fn) {
        return match_arguments(fn->parameters() | TRANSFORM([](const parameter& param) {
                                 return param.type;
                               }) | TO_VEC,
                               argument_types);
      }) |
      TO_VEC;

  if (possible_exact_match.size() == 1) {
    return possible_exact_match.front();
  }

  // Overload resolution
  auto resolved = resolve_overloads(id, overloads, argument_expressions);

  if (resolved == nullptr) {
    THROW(UNDECLARED_SYMBOL, id);
  }
  return resolved;
}

std::optional<assembly::operand*> compilation_unit::call_function(const identifier& id,
                                                                  const ast::expr::arguments& args,
                                                                  bool inlined_) {
  if (asmgen::exists_snippet(id.value())) {
    asmgen.register_snippet(id.value());
    call(id.value());
    return {};
  }

  const auto& fn = get_callable<decl::function>(id, args);

  if (nullptr == fn) {
    throw_error<compilation_error_t::UNDECLARED_SYMBOL>(id);
  }
  if (fn->body == nullptr) {
    throw_error<compilation_error_t::UNDEFINED_FUNCTION>(id);
  }
  auto bounded = bind_parameters(fn->parameters(), args);
  ast->create_frame(fn->body);
  auto params = regs.parameters();
  auto ps     = load_arguments(bounded);
  if (!inlined_) {
    ast->active_frame()->local_stack.push();
    call(id.value());
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

  if (fn->specs.type.value() != types::global().make(types::core_t ::void_t, {})) {
    return regs.get(register_t::ACCUMULATOR);
  };

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
  operand* res       = nullptr;
  auto params        = regs.parameters();
  auto load_argument = [this, &params](expr::expression* e) {
    semantics::load_node_semantics(*e);
    return runner.generate_expr(*e, params.next());
  };
  auto* impls = get_callable<builtin_operator_data>(op, args);
  auto ops    = args | TRANSFORM(load_argument) | TO_VEC;

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

std::vector<bound_argument> compilation_unit::bind_parameters(
    const std::vector<parameter>& parameters,
    const expr::arguments& args) const {
  return std::views::zip_transform(
             [](const parameter& param, expr::expression* arg) -> bound_argument {
               types::type_id param_type      = param.type;
               types::type_id real_param_type = param_type;

               binding_mode_t mode =
                   bind_value(arg->semantics()->value_category.value(), param_type);

               ast::expr::expression* n_expr = param.init == nullptr ? arg : param.init;
               if (n_expr == nullptr) {
                 THROW(BAD_FUNCTION_CALL, arg);
               }
               return std::make_pair(
                   parameter{real_param_type, param.identifier, n_expr, param.decl}, mode);
             },
             parameters,
             args) |
         std::ranges::to<std::vector>();
}

std::vector<operand*> compilation_unit::load_arguments(const std::vector<bound_argument>& bound) {
  auto registers = regs.parameters();
  return bound | std::views::transform([this, &registers](auto& b) {
           auto& [parameter, mode] = b;
           auto* reg               = runner.generate_expr(*parameter.init, registers.next());
           return ast->active_frame()->declare_parameter(*parameter.decl, reg);
         }) |
         std::ranges::to<std::vector>();
}

template <typename T, typename Id>
const T* compilation_unit::resolve_overloads(Id id,
                                             std::vector<const T*> candidates,
                                             const expr::arguments& argument_expressions) const {
  // Already have candidates
  // Lets check number of parameters
  auto check_param_number = [argument_expressions](const T* fn) {
    auto parameters = fn->parameters();
    auto n_params   = parameters.size();
    auto n_args     = argument_expressions.size();
    if (n_params > n_args) {
      auto idx = n_params - (n_params - n_args);
      for (; idx < n_params; ++idx) {
        if (parameters.at(idx).init == nullptr) {
          return false;
        }
      }
      return true;
    }

    if (n_params < n_args) {
      // Ellipsis is not yet implemented
      return false;
    }

    // n_params == n_args
    return true;
  };
  auto step1 = candidates | std::views::filter(check_param_number) | TO_VEC;
  REGISTER_INFO("Tras step1 quedan {}", step1.size());
  auto check_binding_rules = [argument_expressions](const T* fn) {
    // Binding rules
    auto parameters = fn->parameters();
    return std::ranges::all_of(
        std::views::zip(parameters, argument_expressions), [](const auto& pair) {
          const auto& [parameter, expression] = pair;
          // If the parameter is reference, it applies the binding rule
          // Binding rules are supposed to be counted as  a implicit conversions
          // But for now we'll just take into account the implicit conversion rule
          // on those parameter types that are not reference (hence applies the binding rule)
          if (types::is_reference(parameter.type).ok) {
            // If is a lvalue argument with a non const lvalue ref parameter
            bool rvaluearg_nonconstlvalue =
                (types::is_lvalue && types::is_const)(parameter.type).ok &&
                expression->semantics()->value_category == value_category_t::RVALUE;
            // Or lvalue arg with rvalue reference parameter
            bool lvaluearg_rvaluerefparam =
                types::is_rvalue(parameter.type).ok &&
                expression->semantics()->value_category == value_category_t::LVALUE;
            // Is not viable function
            return !rvaluearg_nonconstlvalue && !lvaluearg_rvaluerefparam;
          } else {
            return std::ranges::any_of(standard_conversions, [&expression](const auto& conv) {
              auto* semantics = expression->semantics();
              auto expr_t     = semantics->original_type;
              return conv->is_convertible(expr_t);
            });
          }
        });
  };
  auto step2 = step1 | std::views::filter(check_binding_rules) | TO_VEC;
  REGISTER_INFO("Tras step2 quedan {}", step2.size());
  if (step2.size() == 0) {
    THROW(UNDECLARED_SYMBOL, id);
  }
  return step2.front();
}

template const builtin_operator_data* compilation_unit::resolve_overloads(
    operator_,
    std::vector<const builtin_operator_data*>,
    const expr::arguments&) const;
template const ast::decl::function* compilation_unit::resolve_overloads(
    ast::identifier,
    std::vector<const ast::decl::function*>,
    const expr::arguments&) const;

[[nodiscard]] bool compilation_unit::match_arguments(const std::vector<type_id>& builtin,
                                                     const std::vector<type_id>& called) {
  return std::ranges::all_of(std::views::zip(builtin, called), [](const auto& pair) {
    const auto& [required, actual] = pair;
    return required == actual;
  });
}
// static_assert(std::formattable<operand*, char>);
static_assert(DisplayablePtr<operand*>);

} // namespace cmm::ir
