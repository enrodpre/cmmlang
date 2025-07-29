#include "ir.hpp"
#include "asm.hpp"
#include "ast.hpp"
#include "common.hpp"
#include "lang.hpp"
#include <bits/ranges_algo.h>
#include <format>
#include <libassert/assert.hpp>
#include <magic_enum/magic_enum.hpp>
#include <ranges>
#include <type_traits>
#include <utility>

namespace cmm::ir {

namespace {
  linkage_t parse_linkage(const ast::decl::specifiers& specs) {
    if (auto static_ = std::ranges::find_if(
            specs, [](const auto& spec) { return spec.type == token_t::static_; });
        static_ != specs.end()) {
      return linkage_t::internal;
    }
    return linkage_t::normal;
  }

  storage_t parse_storage(const ast::decl::specifiers& specs) {
    auto storages = specs.data() |
                    std::views::filter([](const auto& spec) { return spec.type.is_storage(); }) |
                    std::ranges::to<std::vector>();
    if (storages.size() == 0) {
      return storage_t::normal;
    }
    if (storages.size() == 1) {
      return storages[0].type.cast<storage_t>();
    }

    throw incompatible_token(storages[1].location(), storages[1].format(), storages[0].format());
  }
  namespace {
    constexpr type_t parse_enum_type(const token_t& token_type, bool unsigned_) {
      if (token_type == token_t::int_t) {
        return unsigned_ ? type_t::uint_t : type_t::sint_t;
      }
      return token_type.cast<type_t>();
    }
  }; // namespace
  cv_type parse_type(const ast::decl::specifiers& specs) {
    bool const_    = false;
    bool volatile_ = false;
    bool unsigned_ = false;
    std::optional<token_t> type_;
    for (const auto& t : specs) {
      if (t.type == token_t::const_) {
        const_ = true;
      } else if (t.type == token_t::volatile_) {
        volatile_ = true;
      } else if (t.type.is_type()) {
        type_.emplace(t.type);
      } else if (t.type == token_t::unsigned_) {
        unsigned_ = true;
      }
    }

    if (!type_.has_value()) {
      auto r = specs |
               std::views::transform([](const auto& spec) -> location { return spec.location(); }) |
               std::ranges::to<std::vector>();
      location l = std::ranges::fold_left(r.cbegin(), r.cend(), location(), std::plus<>());

      throw required_type(l);
    }
    return type_store.retrieve(parse_enum_type(type_.value(), unsigned_), const_, volatile_);
  }

} // namespace

using namespace cmm::assembly;

builtin_function::builtin_function(std::string mang,
                                   std::optional<cv_type> ret,
                                   parameters_t args,
                                   descriptor_t desc,
                                   bool inlined_)
    : function(nullptr, {}, std::move(mang), linkage_t::normal, ret, inlined_),
      parameters(std::move(args)),
      descriptor(std::move(desc)) {}

bool builtin_function::is_defined() const { return true; }

std::string mangled_name::types(const std::vector<cv_type>& types) {
  return types | std::views::transform([](cv_type t) -> std::string { return t->format(); }) |
         std::views::join_with('_') | std::ranges::to<std::string>();
}
mangled_name mangled_name::builtin_function(std::string name, const std::vector<cv_type>& t) {
  return std::format("{}_{}", name, types(t));
}

label::label(const ast::decl::label* label_, address_t addr)
    : symbol(label_, addr) {}

[[nodiscard]] std::string label::format() const { return std::format("label({})", decl->term); }

static_assert(std::formattable<ast::term::identifier, char>);

variable::variable(scope& scope,
                   const ast::decl::variable* p,
                   address_t addr,
                   linkage_t l,
                   storage_t s,
                   cv_type t)
    : symbol(p, addr),
      linkage(l),
      storage(s),
      type(t),
      scope_ref(scope) {}

global_variable::global_variable(scope& scope,
                                 const ast::decl::variable* decl,
                                 assembly::label_memory* addr,
                                 cv_type t)
    : variable(scope, decl, addr, parse_linkage(decl->specifiers), storage_t::static_, t) {}

local_variable::local_variable(local_scope& scope,
                               const ast::decl::variable* decl,
                               operand* addr,
                               storage_t s,
                               cv_type t)
    : variable(scope, decl, addr, linkage_t::normal, s, t) {}

auto_local_variable::auto_local_variable(local_scope& scope,
                                         const ast::decl::variable* decl,
                                         assembly::reg_memory* r,
                                         storage_t s,
                                         cv_type t)
    : local_variable(scope, decl, r, s, t) {}

arg_local_variable::arg_local_variable(local_scope& scope,
                                       const ast::decl::variable* decl,
                                       assembly::reg* r,
                                       storage_t s,
                                       cv_type t)
    : local_variable(scope, decl, r, s, t) {}

[[nodiscard]] std::string variable::format() const {
  return std::format("var({}, {}, {}, {})", decl->ident->value, linkage, storage, type->format());
}

function::function(const ast::decl::function* decl,
                   address_t addr,
                   std::string id,
                   linkage_t link,
                   return_t t,
                   bool inlined_)
    : symbol(decl, addr),
      identifier(std::move(id)),
      linkage(link),
      return_type(t),
      inlined(inlined_) {}

FORMAT_IMPL(function, "function<{}>", identifier);

operand* builtin_function::run(compilation_unit& ctx, ast::expr::call::arguments args) const {
  // PREP
  auto prep_b = ctx.asmgen.begin_comment_block("preprocess {}", identifier);
  auto regs   = descriptor.preprocess(ctx, parameters, args);
  prep_b.end();

  // BODY
  return run(ctx, regs);
}

operand* builtin_function::run(compilation_unit& ctx, std::vector<operand*> regs) const {
  auto exec_b    = ctx.asmgen.begin_comment_block("executing body {}", identifier);

  auto* res_body = descriptor.body(ctx, regs);
  exec_b.end();

  // POST
  auto post_b = ctx.asmgen.begin_comment_block("postprocess {}", identifier);
  auto res    = descriptor.postprocess(ctx, res_body);
  if constexpr (ReturnsVoid<std::invoke_result_t<builtin_function::postprocess_t,
                                                 compilation_unit&,
                                                 operand*>>) {};
  return res.value();
}

user_function::user_function(const ast::decl::function* decl,
                             address_t addr,
                             linkage_t link,
                             cv_type t,
                             bool inlined_)
    : function(decl, addr, decl->ident.value, link, t, inlined_),
      body(decl->body),
      parameters(decl->parameters) {}

operand* user_function::run(compilation_unit& ctx, ast::expr::call::arguments args) const {
  REGISTER_TRACE("Calling {}", decl->ident);

  // Load parameters
  auto prepared_args =
      std::views::zip(decl->parameters, args) | std::views::enumerate |
      std::views::transform([this, &ctx](const auto& enumerated_pair) -> operand* {
        const auto& [i, pair]         = enumerated_pair;
        const auto& [parameter, expr] = pair;

        auto* actual_expr             = expr != nullptr ? expr : parameter.init;
        if (actual_expr == nullptr) {
          throw wrong_function_argument(decl->location(), parameter.format(), "nullptr");
        }

        const auto* expr_type = ctx.get_expression_type(*actual_expr);

        if (ctx.table.is_declared<variable>(*parameter.ident)) {
          throw already_declared_symbol(parameter);
        }

        auto* reg_ = ctx.regs.parameters.at(i);
        return ctx.runner.generate_expr(*actual_expr, intents::intent_t::LOAD_VARIABLE_VALUE, reg_);
      }) |
      std::ranges::to<std::vector>();

  return run(ctx, prepared_args);
}

user_function::address_t user_function::run(compilation_unit& ctx,
                                            std::vector<operand*> ops) const {

  ctx.table.push_frame(decl->ident);

  for (const auto& [param, op] : std::views::zip(decl->parameters, ops)) {
    ctx.table.active_frame().active_scope().emplace_argument(&param, dynamic_cast<reg*>(op));
  }

  if (!inlined) {
    // The rsp stored in the stack
    ctx.table.active_frame().local_stack.push();
    ctx.call(decl->ident.value);
    ctx.asmgen.create_delay();
  }

  ctx.runner.generate_statements(*body);

  if (!inlined) {
    auto code = ctx.asmgen.dump_delayed();
    ctx.asmgen.register_labeled_code_block(decl->ident.value, std::move(code));
  }

  size_t ditched = ctx.table.pop_frame();
  // +1 because we pushed one due to function calling (ret address is pushed
  // onto the stack)
  if (!ctx.table.is_global_scope()) {
    ctx.table.active_frame().local_stack.pop(ditched + 1);
  }

  return ctx.regs.get(registers::ACCUMULATOR);
}

bool user_function::is_defined() const { return body != nullptr; }

using parameters_t = ast::decl::function::parameters_t;
using arguments_t  = ast::expr::call::arguments;

function_store::function_store()
    : formattable_range(&m_store) {}

local_scope::local_scope(const frame& frame, const ast::compound& comp)
    : frame_ref(frame),
      compound(comp) {}

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

mangled_name mangled_name::free_unary_operator(const operator_t& op, cstring expr_t) {
  std::ostringstream os;
  os << op.caller_function();
  os << "_";
  os << expr_t;
  return os.str();
}

mangled_name mangled_name::free_binary_operator(const operator_t& op, cstring l, cstring r) {
  std::ostringstream os;
  os << op.caller_function();
  os << "_";
  os << l;
  os << "_";
  os << r;
  return os.str();
}

mangled_name mangled_name::free_function(const ast::decl::function* fn) {
  if (fn->parameters.empty()) {
    return mangled_name{fn->ident.value};
  }

  auto mangled_params = fn->parameters.data() |
                        std::views::transform([](const auto& p) -> std::string {
                          return parse_type(p.specifiers)->format();
                        }) |
                        std::views::join_with('_');
  // auto joined = std::format("{}", std::join(mangled_params, "_"));
  return std::string(fn->ident.value); // + "_" + joined;
}

mangled_name::operator std::string() const { return m_string; }

using var_store = variable_store;
var_store::variable_store()
    : formattable_range(&m_store) {}

bool var_store::contains(const std::string& id) const { return m_store.contains(id); }

var_store::value_type* var_store::get(const std::string& id) { return &m_store.at(id); }
[[nodiscard]] const var_store::value_type* var_store::get(const std::string& id) const {
  return &m_store.at(id);
}
size_t var_store::size() const noexcept { return m_store.size(); }

void var_store::clear() { m_store.clear(); }

operand* local_scope::emplace_argument(const ast::decl::variable* decl, reg* r) {
  auto var = variables.emplace<local_variable>(
      decl->ident->value, *this, decl, r, storage_t::normal, parse_type(decl->specifiers));
  r->hold_value(&var);
  return r;
}

operand* local_scope::emplace_automatic(const ast::decl::variable* decl) {
  auto* addr =
      operand_factory::instance().create<stack_memory>(frame_ref.get().local_stack.size() + 1);
  auto var = variables.emplace<local_variable>(
      decl->ident->value, *this, decl, addr, storage_t::normal, parse_type(decl->specifiers));
  addr->hold_address(&var);
  return addr;
}

operand* global_scope::emplace_static(const ast::decl::variable* decl) {
  auto* addr = operand_factory::instance().create<label_memory>(decl->label());
  auto var   = variables.emplace<global_variable>(
      decl->ident->value, *this, decl, addr, parse_type(decl->specifiers));
  addr->hold_address(&var);
  return addr;
}

using fn_store = function_store;
bool fn_store::contains(const mangled_name& mname) const { return m_store.contains(mname); }

fn_store::value_type::pointer fn_store::get(const mangled_name& mname) {
  return m_store.at(mname).get();
}

std::vector<fn_store::value_type::pointer> fn_store::get(cstring id) const {
  // TODO support overload
  return m_store | std::views::filter([id](const auto& pair) -> bool {
           const auto& [k, v] = pair;
           return k.starts_with(id);
         }) |
         std::views::transform([](const auto& pair) -> fn_store::value_type::pointer {
           const auto& [k, v] = pair;
           return v.get();
         }) |
         std::ranges::to<std::vector>();
}

const function* fn_store::emplace_builtin(const mangled_name& mang,
                                          std::optional<cv_type> ret,
                                          const std::vector<cv_type>& params,
                                          const builtin_function::descriptor_t& desc,
                                          bool inline_) {
  auto mang_str = mang.str();
  auto ptr      = std::make_unique<builtin_function>(mang_str, ret, params, desc, inline_);
  return m_store.emplace(mang_str, std::move(ptr)).first->second.get();
}
const function* fn_store::emplace_user_provided(const ast::decl::function* decl, bool inline_) {
  auto mang = mangled_name::free_function(decl);
  return m_store
      .emplace(mang.str(),
               std::make_unique<user_function>(
                   decl,
                   operand_factory::instance().create<assembly::label>(mang.str()),
                   parse_linkage(decl->specifiers),
                   parse_type(decl->specifiers),
                   inline_))
      .first->second.get();
}

void fn_store::clear() { m_store.clear(); }

frame::frame(const user_function* fn)
    : func(*fn) {
  create_scope(*fn->body);
}

void frame::clear() noexcept {
  while (!scopes.empty()) {
    destroy_scope();
  }
  scopes.clear();
}

local_scope& frame::active_scope() {
  DEBUG_ASSERT(!scopes.empty());
  return scopes.top();
}
[[nodiscard]] const local_scope& frame::active_scope() const {
  DEBUG_ASSERT(!scopes.empty());
  return scopes.top();
}

[[nodiscard]] bool frame::is_declared(const ast::term::identifier& ident) const noexcept {
  return std::ranges::any_of(scopes.cbegin(), scopes.cend(), [ident](const scope& scope_) {
    return scope_.variables.contains(ident.value);
  });
}

[[nodiscard]] variable* frame::get(const ast::term::identifier& ident) {
  if (!is_declared(ident)) {
    throw undeclared_symbol(ident);
  }

  for (auto& scope : scopes) {
    if (scope.variables.contains(ident.value)) {
      return scope.variables.get(ident.value);
    }
  }
  UNREACHABLE();
}

[[nodiscard]] const variable* frame::get(const ast::term::identifier& ident) const {
  if (!is_declared(ident)) {
    throw undeclared_symbol(ident);
  }

  for (const auto& scope : scopes) {
    if (scope.variables.contains(ident.value)) {
      return scope.variables.get(ident.value);
    }
  }

  UNREACHABLE("Unrecheable");
}

void symbol_table::push_frame(id ident) {
  const auto* func = dynamic_cast<const user_function*>(get_function(ident));
  m_stackframe.emplace_back(func);
  active_frame().create_scope(*func->body);
}
static_assert(std::is_constructible_v<frame, const ir::user_function*>);
static_assert(std::is_class_v<frame>);

size_t symbol_table::pop_frame() {
  DEBUG_ASSERT(!is_global_scope());
  size_t ditched = 0;
  for (int i = 0; i < active_frame().scopes.size(); ++i) {
    ditched += active_frame().destroy_scope();
  }
  m_stackframe.pop();
  return ditched;
};

void frame::create_scope(const ast::compound& comp) noexcept { scopes.emplace_back(*this, comp); }

size_t frame::destroy_scope() noexcept {
  size_t ditched = scopes.top().variables.size();
  DEBUG_ASSERT(!scopes.empty());
  scopes.pop();
  REGISTER_TRACE("Cleared local scope: ditched {} elements", ditched);
  return ditched;
}

void symbol_table::clear() noexcept {
  m_stackframe.clear();
  m_functions.clear();
  m_global_scope.variables.clear();
}

[[nodiscard]] frame& symbol_table::active_frame() noexcept {
  DEBUG_ASSERT(!m_stackframe.empty());
  return m_stackframe.top();
}

[[nodiscard]] const frame& symbol_table::active_frame() const noexcept {
  DEBUG_ASSERT(!m_stackframe.empty());
  return m_stackframe.top();
}

[[nodiscard]] scope& symbol_table::active_scope() noexcept {
  if (m_stackframe.empty()) {
    return m_global_scope;
  }
  return active_frame().active_scope();
}
[[nodiscard]] const scope& symbol_table::active_scope() const noexcept {
  if (m_stackframe.empty()) {
    return m_global_scope;
  }
  return active_frame().active_scope();
}
bool symbol_table::is_entry_point_defined() const noexcept { return m_entry_point != nullptr; }

std::string symbol_table::format() const {
  return "";
  // m_global_scope.variables.join(", ");
}

const label* symbol_table::get_label(id ident) const {
  return &active_frame().labels.at(ident.value);
}

const variable* symbol_table::get_variable(id ident) const {
  if (m_global_scope.variables.contains(ident.value)) {
    return m_global_scope.variables.get(ident.value);
  }

  return active_frame().get(ident);
}
const function* symbol_table::get_function(const mangled_name& mname) const {
  const auto& candidates = m_functions.get(mname.str());
  ASSERT(candidates.size() == 1);
  return candidates[0];
}

const function* symbol_table::get_function(id ident) const {
  if (ident.value == "main") {
    return m_entry_point.get();
  }

  const auto& candidates = m_functions.get(ident.value);
  if (candidates.size() < 1) {
    throw undeclared_symbol(ident);
  }

  return candidates[0];
}
void symbol_table::declare_function(const ast::decl::function* func, bool inline_) {
  REGISTER_TRACE("Creating func {}", func->ident);
  if (m_functions.contains(mangled_name::free_function(func))) {
    throw already_declared_symbol(func->ident);
  }

  m_functions.emplace_user_provided(func, inline_);
}

user_function* symbol_table::get_entry_point() { return m_entry_point.get(); }

void symbol_table::link_entry_point(const ast::decl::function* fn) {
  if (!is_entry_point_defined()) {
    m_entry_point =
        std::make_unique<user_function>(fn,
                                        operand_factory::instance().create<assembly::label>("main"),
                                        parse_linkage(fn->specifiers),
                                        parse_type(fn->specifiers),
                                        true);
  } else {
    throw already_declared_symbol(*fn);
  }
}

bool symbol_table::is_global_scope() const noexcept { return m_stackframe.empty(); }

bool symbol_table::in_main() const noexcept {
  return m_stackframe.top().func.get().decl->ident.value == "main";
}

operand* compilation_unit::call_builtin(const std::string&, std::vector<operand*>) {
  // const auto* fn      = table.get_function(ast::term::identifier(name));
  // const auto* builtin = dynamic_cast<const builtin_function*>(fn);
  // return fn->run(*this, std::move(ops));
  return nullptr;
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
  size_t line_n = current_statement->location().rows.start;
  return source->get_line(line_n);
}

const variable* compilation_unit::declare_global_variable(const ast::decl::variable& decl,
                                                          operand* init) {
  ASSERT(table.is_global_scope());
  reserve_memory(decl.label(), "resq", "1");
  auto* addr = table.m_global_scope.emplace_static(&decl);
  move(addr, init);
  return addr->variable();
}

const variable* compilation_unit::declare_variable(const ast::decl::variable& decl, operand* init) {
  // + 1 because not counted yet
  const auto* var = table.active_frame().active_scope().emplace_automatic(&decl)->variable();
  push(init);
  return var;
}

operand* compilation_unit::get_variable_address(id ident) const {
  const auto* var = table.get_variable(ident);
  auto* addr      = var->addr;
  addr->hold_address(var);
  return addr;
}

void compilation_unit::save_variable(const variable* var, operand* r) {
  asmgen.write_instruction(_instruction_t::mov, var->addr->value(), r->value());
}

void compilation_unit::declare_label(const ast::decl::label& l) {
  if (table.is_global_scope()) {
    throw label_in_global(l.term);
  }

  table.active_frame().labels.emplace(
      std::piecewise_construct,
      std::forward_as_tuple(l.term.value),
      std::forward_as_tuple(&l, operand_factory::instance().create<assembly::label>(l.term.value)));
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
    asmgen.write_instruction(_instruction_t::mov, to->value(), from->value());
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
  asmgen.add_section_data(assembly::asmgen::Section::BS, name, kw, times);
}
cv_type compilation_unit::get_expression_type(const ast::expr::expression& e) {
  if (const auto* id = dynamic_cast<const ast::expr::identifier*>(&e)) {
    // TODO
    return table.get_variable(id->term)->type;
  }

  if (const auto* lit = dynamic_cast<const ast::expr::literal*>(&e)) {
    return type_store.retrieve(lit->type);
  }

  if (const auto* unary = dynamic_cast<const ast::expr::unary_operator*>(&e)) {
    const auto* expr_t = get_expression_type(unary->expr);
    auto mangled       = mangled_name::free_unary_operator(unary->operator_.type, expr_t->format());
    return *table.get_function(mangled)->return_type;
  }

  if (const auto* bin = dynamic_cast<const ast::expr::binary_operator*>(&e)) {
    const auto* left_t  = get_expression_type(bin->left);
    const auto* right_t = get_expression_type(bin->right);
    auto mangled        = mangled_name::free_binary_operator(
        bin->operator_.type, left_t->format(), right_t->format());
    return *table.get_function(mangled)->return_type;
  }

  if (const auto* call = dynamic_cast<const ast::expr::call*>(&e)) {
    auto types = call->args | std::views::transform([this](const auto& expr) -> cv_type {
                   return get_expression_type(*expr);
                 }) |
                 std::ranges::to<std::vector>();
    auto mangled = mangled_name::builtin_function(call->ident.value, types);
    return *table.get_function(mangled)->return_type;
  }

  throw generic_error(e);
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

void compilation_unit::exit(operand* op) { call_builtin("exit", {op}); }

void compilation_unit::exit(size_t exit_code) {
  auto* arg = move_immediate(regs.parameters.at(0), std::to_string(exit_code));
  call_builtin("exit", {arg});
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
    throw missing_entry_point(location());
  }

  /* if (m_state.current_phase != Phase::EXITING) { */
  // If does not have changed the phase,
  // it means that it hasnt exited or returned
  if (current_phase != Phase::EXITING) {
    move_immediate(regs.get(registers::ACCUMULATOR), "0");
    exit(60);
  }

  asmgen.write_label("exit");
  syscall("60");

  REGISTER_TRACE("Code generation finished");
  return asmgen.end();
}

static_assert(std::formattable<operand*, char>);

} // namespace cmm::ir
