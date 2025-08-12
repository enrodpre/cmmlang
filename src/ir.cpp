#include "ir.hpp"
#include "allocator.hpp"
#include "asm.hpp"
#include "ast.hpp"
#include "common.hpp"
#include "lang.hpp"
#include "types.hpp"
#include <algorithm>
#include <bits/ranges_algo.h>
#include <format>
#include <libassert/assert.hpp>
#include <magic_enum/magic_enum.hpp>
#include <optional>
#include <ranges>
#include <tuple>

#include <utility>
namespace cmm::ir {

using namespace cmm::assembly;
using namespace cmm::ast;

[[nodiscard]] std::vector<ptr_type> function::argument_types() const {
  return declaration()->params |
         std::views::transform([](const ast::decl::function::parameter& var) -> ptr_type {
           return &var.specs.type.type_;
         }) |
         std::ranges::to<std::vector>();
}

FORMAT_IMPL(function, "function<{}>", identifier());

std::optional<operand*> function::run(compilation_unit& ctx,
                                      const ast::expr::call::arguments& args) const {
  struct visitor {
    visitor(compilation_unit& v, decltype(args) a, const function& f)
        : v(v),
          a(f.declaration()->params.load_arguments(a)),
          fn(f) {}
    compilation_unit& v;
    loaded_parameters a;
    const function& fn;
    std::optional<operand*> operator()(ast::scope::block* b) {
      for (const auto& param : a) {
        auto* reg = v.runner.generate_expr(*param.init, param.specs.type, v.regs.parameters.next());
        v.table.active_frame().active_scope().create_variable(&param,
                                                              dynamic_cast<assembly::reg*>(reg));
      }

      if (!fn.inlined) {
        // The rsp stored in the stack
        v.table.active_frame().local_stack.push();
        v.call(fn.identifier());
        v.asmgen.create_delay();
      }

      v.runner.generate_statements(*b);

      if (!fn.inlined) {
        auto code = v.asmgen.dump_delayed();
        v.asmgen.register_labeled_code_block(fn.identifier(), std::move(code));
      }

      size_t ditched = v.table.pop_frame();
      // +1 because we pushed one due to function calling (ret address is pushed
      // onto the stack)
      if (!v.table.is_global_scope()) {
        v.table.active_frame().local_stack.pop(ditched + 1);
      }

      if (fn.declaration()->specs.type.type_ != *VOID_T) {
        return v.regs.get(registers::ACCUMULATOR);
      }
      return {};
    }
    std::optional<operand*> operator()(const builtin_body& b) {
      // PREP
      auto prep_b = v.asmgen.begin_comment_block("preprocess {}", fn.declaration()->ident.value());
      auto ops    = b.pre(v, a);
      auto* res   = b.body(v, ops);
      return b.post(v, res);
    }
  };

  visitor vis(ctx, args, *this);
  return std::visit(vis, body);
}

using parameters_t = ast::decl::function::parameters;
using arguments_t  = ast::expr::call::arguments;

function_store::function_store()
    : formattable_range(&m_store) {}

operand* scope::create_variable(const ast::decl::variable* p, operand* o) {
  auto& res = variables.emplace<variable>(p->ident.value(), p);
  res.load(this, o);
  o->hold_value(&res);
  return o;
}

local_scope::local_scope(const frame& frame, const ast::scope::block& comp)
    : frame_ref(frame),
      compound(comp) {}

void local_scope::load_argument(variable var) { variables.put(std::move(var)); }
// const frame* local_scope::parent() const { return &frame_ref.get(); }

using id = symbol_table::identifier_type;

symbol_table::symbol_table()
    : m_entry_point(nullptr),
      m_global_scope() {
  builtin::provider(*this).provide();

} // namespace cmm::ir

mangled_name::mangled_name(const std::string& str)
    : m_string(str) {}

mangled_name::mangled_name(std::string&& str)
    : m_string(std::move(str)) {}

[[nodiscard]] const std::string& mangled_name::str() const { return m_string; }

mangled_name::operator std::string() const { return m_string; }

std::optional<operand*> compilation_unit::call_builtin(
    const builtin::function::builtin_signature_t& header,
    const std::vector<operand*>& args) {
  const auto* fn = table.get_function(header.signature());
  // return fn->run(*this, args);
}

compilation_unit::compilation_unit()
    : runner(*this) {}

std::string compilation_unit::compile(ast::program& p, const source_code* src) {
  source = src;
  start();
  runner.generate_program(p);
  return end();
}

std::string compilation_unit::current_line() const {
  size_t line_n = current_statement->location()->rows.start;
  return source->get_line(line_n);
}

const variable* compilation_unit::declare_global_variable(const ast::decl::variable& decl,
                                                          operand* init) {
  ASSERT(table.is_global_scope());
  reserve_memory(decl.ident.format(), "resq", "1");
  auto* addr = table.m_global_scope.create_variable(
      &decl, operand_factory::instance().create<label_memory>(decl.ident.value()));
  move(addr, init);
  return addr->variable();
}

const variable* compilation_unit::declare_local_variable(const ast::decl::variable& decl,
                                                         operand* init) {
  // + 1 because not counted yet
  const auto* var = table.active_frame()
                        .active_scope()
                        .create_variable(&decl,
                                         operand_factory::instance().create<stack_memory>(
                                             table.active_frame().local_stack.size() + 1))
                        ->variable();
  push(init);
  return var;
}

void compilation_unit::create_frame(const ast::scope::function& fn) {
  table.m_stackframe.emplace_back(fn);
}

operand* compilation_unit::get_variable_address(id ident) const {
  const auto* var = table.get_variable(ident);
  auto* addr      = var->address;
  addr->hold_address(var);
  return addr;
}

void compilation_unit::save_variable(const variable* var, operand* r) {
  asmgen.write_instruction(_instruction_t::mov, var->address->value(), r->value());
}

void compilation_unit::declare_label(const ast::decl::label& l) {
  if (table.is_global_scope()) {
    throw_error<_error_t::LABEL_IN_GLOBAL>(l.term);
  }

  table.active_frame().labels.emplace(
      std::piecewise_construct,
      std::forward_as_tuple(l.term.value()),
      std::forward_as_tuple(&l,
                            &table.active_frame().active_scope(),
                            operand_factory::instance().create<label_memory>(l.term.value())));
}

void compilation_unit::push(operand* r) {
  asmgen.write_instruction(_instruction_t::push, r->value());
  table.active_frame().local_stack.push();
}
void compilation_unit::pop(operand* r) {
  asmgen.write_instruction(_instruction_t::pop, r->value());
  table.active_frame().local_stack.pop();
}
void compilation_unit::call(cstring func) { asmgen.write_instruction(_instruction_t::call, func); }
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
  asmgen.write_instruction(_instruction_t::mov, r->value(), lit);
  return r;
}

operand* compilation_unit::lea(operand* to, operand* from) {
  asmgen.write_instruction(_instruction_t::lea, to->value(), from->value());
  to->hold_address(from->variable());
  return to;
}

operand* compilation_unit::zero(operand* reg) {
  asmgen.write_instruction(_instruction_t::xor_, reg->value(), reg->value());
  return reg;
}

void compilation_unit::reserve_memory(cstring name, cstring kw, cstring times) {
  asmgen.add_bss(name, kw, times);
}

assembly::label_literal* compilation_unit::reserve_constant(cstring value) {
  std::string name = std::format("str_{}", counters.literals++);
  asmgen.add_data(name, value);
  return operand_factory::instance().create<assembly::label_literal>(name);
}

void compilation_unit::jump(cstring label) { asmgen.write_instruction(_instruction_t::jmp, label); }
void compilation_unit::jump(const instruction_t& ins, cstring a) {
  asmgen.write_instruction(ins, a);
}

void compilation_unit::move_rsp(size_t n) {
  asmgen.write_instruction(instruction_t::sub, "rsp", n * DATASIZE);
  table.active_frame().local_stack.pop(n);
}

void compilation_unit::cmp(cstring a, cstring b) {
  asmgen.write_instruction(_instruction_t::cmp, a, b);
}
void compilation_unit::ret() { asmgen.write_instruction(_instruction_t::ret); }
using namespace builtin::function;

void compilation_unit::exit(operand* op) { call_builtin(builtin_signature_t::EXIT, {op}); }

void compilation_unit::exit_successfully() {
  auto* reg = regs.parameters.next();
  auto* arg = move_immediate(reg, "0");
  exit(reg);
}

void compilation_unit::syscall() { asmgen.write_instruction(_instruction_t::syscall); }
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
std::string compilation_unit::end() {
  if (!table.is_entry_point_defined()) {
    throw_error<_error_t::MISSING_ENTRY_POINT>();
  }

  REGISTER_TRACE("Code generation finished");
  return asmgen.end();
}

static_assert(std::formattable<operand*, char>);

} // namespace cmm::ir
