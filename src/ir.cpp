#include "ir.hpp"
#include "asm.hpp"
#include "ast.hpp"
#include "common.hpp"
#include "ir.inl"
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

builtin_function::builtin_function(cr_string name,
                                   ptr_type ret,
                                   const parameters_t& args,
                                   descriptor_t&& desc,
                                   bool inlined_)
    : function(nullptr, linkage_t::normal, ret, inlined_),
      signature({name, args}),
      descriptor(std::move(desc)) {}

builtin_function::builtin_function(signature_t&& s, ptr_type t, descriptor_t&& d, bool i)
    : function(nullptr, linkage_t::normal, t, i),
      signature(std::move(s)),
      descriptor(std::move(d)) {}

builtin_function::descriptor_t::descriptor_t(body_t b)
    : pre(builtin::function::preprocessors::SIMPLE_LOADING),
      body(std::move(b)),
      post(builtin::function::postprocessors::SIMPLE_RET) {}
builtin_function::descriptor_t::descriptor_t(preprocess_t t, body_t b)
    : pre(std::move(t)),
      body(std::move(b)),
      post(builtin::function::postprocessors::SIMPLE_RET) {}
builtin_function::descriptor_t::descriptor_t(preprocess_t t, body_t b, postprocess_t p)
    : pre(std::move(t)),
      body(std::move(b)),
      post(std::move(p)) {}

bool builtin_function::is_defined() const { return true; }

mangled_name mangled_name::function(cstring name, const std::vector<ast::decl::variable>& vars) {
  auto args = vars | std::views::transform([](const auto& var) -> ptr_type {
                return &var.specifiers.parse_type();
              }) |
              std::ranges::to<std::vector>();
  return function(name, args);
}

mangled_name mangled_name::function(cstring name, const std::vector<const type*>& t) {
  return std::format("{}_{}", name, types(t));
}

std::string mangled_name::types(const std::vector<ptr_type>& types) {
  return types | std::views::transform([](ptr_type t) { return t->format(); }) |
         std::views::join_with('_') | std::ranges::to<std::string>();
}

mangled_name mangled_name::direct_conversion_function(cr_type f, cr_type t) {
  return function("conv_{}", std::vector<ptr_type>{&f, &t});
}

mangled_name mangled_name::label(cstring name) { return std::string(name); }
mangled_name mangled_name::variable(cstring name, cr_type) { return std::string(name); }

label::label(const ast::decl::label* label_, local_scope* s, assembly::label_memory* o)
    : symbol(label_) {
  address = o;
  scope   = s;
}

[[nodiscard]] std::string label::format() const {
  return std::format("label({})", declaration()->term);
}

static_assert(std::formattable<ast::term::identifier, char>);

variable::variable(const ast::decl::variable* decl, linkage_t l, storage_t s, ptr_type t)
    : symbol(decl),
      linkage(l),
      storage(s),
      type(t) {}

[[nodiscard]] std::string variable::format() const {
  return std::format("var({}, {}, {}, {})", identifier(), linkage, storage, type);
}

function::function(const ast::decl::function* p, linkage_t link, return_t t, bool inlined_)
    : symbol(p),
      linkage(link),
      return_type(t),
      inlined(inlined_) {}

std::optional<operand*> function::load_and_run(
    compilation_unit& v,
    const std::vector<ast::expr::expression*>& arg) const {
  auto args = load(v, arg);
  return run(v, args);
}

[[nodiscard]] std::vector<ptr_type> builtin_function::argument_types() const {
  return signature.argument_types;
}
[[nodiscard]] std::vector<ptr_type> user_function::argument_types() const {
  return declaration()->parameters | std::views::transform([](const ast::decl::variable& var) {
           return &var.specifiers.parse_type();
         }) |
         std::ranges::to<std::vector>();
}

FORMAT_IMPL(function, "function<{}>", identifier());

std::vector<operand*> builtin_function::load(
    compilation_unit& ctx,
    const std::vector<ast::expr::expression*>& args) const {
  // PREP
  auto prep_b = ctx.asmgen.begin_comment_block("preprocess {}", signature.mangle().str());
  return descriptor.pre(ctx, signature.argument_types, args);
}

std::optional<operand*> builtin_function::run(compilation_unit& ctx,
                                              const std::vector<operand*>& regs) const {
  auto exec_b    = ctx.asmgen.begin_comment_block("executing body {}", signature.mangle().str());

  auto* res_body = descriptor.body(ctx, regs);
  exec_b.end();

  // POST
  return descriptor.post(ctx, res_body);
}

user_function::user_function(const ast::decl::function* decl,
                             linkage_t link,
                             cr_type t,
                             bool inlined_)
    : function(decl, link, &t, inlined_),
      body(decl->body) {}

std::vector<operand*> user_function::load(compilation_unit& ctx,
                                          const std::vector<ast::expr::expression*>& args) const {
  REGISTER_TRACE("Calling {}", identifier());

  ctx.table.push_frame(this);
  // Load parameters
  return std::views::zip(declaration()->parameters, args) | std::views::enumerate |
         std::views::transform([this, &ctx](const auto& enumerated_pair) -> operand* {
           auto& [i, pair]               = enumerated_pair;
           ast::decl::variable parameter = std::get<0>(pair);
           auto* expr                    = std::get<1>(pair);

           auto* actual_expr             = expr != nullptr ? expr : parameter.init;
           if (actual_expr == nullptr) {
             // throw_error<_error_t::wrong_function_argument>(decl->location(), parameter.format(),
             // "nullptr");
             throw std::exception();
           }

           if (!ctx.table.is_declarable<variable>(*parameter.ident)) {
             throw_error<_error_t::ALREADY_DECLARED_SYMBOL>(*parameter.ident);
           }

           auto* reg_ = ctx.runner.generate_expr(
               *actual_expr, intents::intent_t::LOAD_VARIABLE_VALUE, ctx.regs.parameters.next());
           // parameter.load(&ctx.table.active_frame().active_scope(), reg_);
           return reg_;
         }) |
         std::ranges::to<std::vector>();
}

std::optional<operand*> user_function::run(compilation_unit& ctx,
                                           const std::vector<operand*>& ops) const {
  for (auto&& [param, op] : std::views::zip(declaration()->parameters, ops)) {
    const auto* ptr = &param;
    ctx.table.active_frame().active_scope().create_user_provided(ptr,
                                                                 dynamic_cast<assembly::reg*>(op));
  }

  if (!inlined) {
    // The rsp stored in the stack
    ctx.table.active_frame().local_stack.push();
    ctx.call(identifier());
    ctx.asmgen.create_delay();
  }

  ctx.runner.generate_statements(*body);

  if (!inlined) {
    auto code = ctx.asmgen.dump_delayed();
    ctx.asmgen.register_labeled_code_block(identifier(), std::move(code));
  }

  size_t ditched = ctx.table.pop_frame();
  // +1 because we pushed one due to function calling (ret address is pushed
  // onto the stack)
  if (!ctx.table.is_global_scope()) {
    ctx.table.active_frame().local_stack.pop(ditched + 1);
  }

  if (return_type != VOID_T) {
    return ctx.regs.get(registers::ACCUMULATOR);
  }
  return {};
}

bool user_function::is_defined() const { return body != nullptr; }
conversion_function::conversion_function(body_t body)
    : type(conversion_type_t::IMPLICIT),
      body(std::move(body)) {}

operand* conversion_function::operator()(operand* reg) const noexcept {
  cr_type from = reg->content_type();
  ASSERT(is_convertible(from));
  cr_type to_t = to(from);
  struct conversion_visitor {
    cr_type from;
    cr_type to;
    operand* in;
    operand* operator()(std::monostate) const { return in; }
    operand* operator()(const builtin_function::body_t& body) const {
      builtin_function fn("conv", &to, {&from}, {body}, true);
      return fn.run(ir::compilation_unit::instance(), {in}).value();
    }
    operand* operator()(const ast::compound*) const {
      NOT_IMPLEMENTED
      // user_function fn ()
      return nullptr;
    }
  };

  return std::visit(conversion_visitor{.from = from, .to = to_t, .in = reg}, body);
}
direct_conversion_function::direct_conversion_function(body_t body, cr_type from, cr_type to)
    : conversion_function(std::move(body)),
      from_type(from),
      to_type(to) {}
glob_conversion_function::glob_conversion_function(std::string desc,
                                                   body_t body,
                                                   condition_t&& from,
                                                   extractor_t&& to)
    : conversion_function(std::move(body)),
      description(std::move(desc)),
      condition(std::move(from)),
      extractor(std::move(to)) {}

using parameters_t = ast::decl::function::parameters_t;
using arguments_t  = ast::expr::call::arguments;

function_store::function_store()
    : formattable_range(&m_store) {}

operand* scope::create(const ast::decl::variable* p,
                       linkage_t l,
                       storage_t s,
                       ptr_type t,
                       operand* o) {
  auto& res = variables.emplace<variable>(p->ident->value(), p, l, s, t);
  res.load(this, o);
  o->hold_value(&res);
  return o;
}

operand* scope::create_user_provided(const ast::decl::variable* v, operand* o) {
  return create(v,
                v->specifiers.parse_linkage(),
                v->specifiers.parse_storage(),
                &v->specifiers.parse_type(),
                o);
}

operand* scope::create_builtin(const ast::decl::variable* p,
                               linkage_t l,
                               storage_t s,
                               ptr_type t,
                               operand* o) {
  return create(p, l, s, t, o);
}

local_scope::local_scope(const frame& frame, const ast::compound& comp)
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

using var_store = variable_store;
var_store::variable_store()
    : formattable_range(&m_store) {}

bool var_store::contains(const std::string& id) const { return m_store.contains(id); }

var_store::value_type* var_store::get(const std::string& id) { return &m_store.at(id); }
[[nodiscard]] const var_store::value_type* var_store::get(const std::string& id) const {
  return &m_store.at(id);
}
size_t var_store::size() const noexcept { return m_store.size(); }
void var_store::put(value_type&& v) {
  m_store.insert(std::make_pair(v.identifier(), std::move(v)));
}

void var_store::clear() { m_store.clear(); }

const function* function_store::get(const function::signature_t& sig) const {
  auto mangled = sig.mangle();
  if (m_store.contains(mangled)) {
    return m_store.at(mangled).get();
  }
  return nullptr;
}

std::vector<const function*> function_store::get_by_name(cstring name) const {
  return m_store | std::views::filter([name](const auto& pair) -> bool {
           const auto& [k, v] = pair;
           return k.starts_with(name);
         }) |
         std::views::transform(
             [name](const auto& pair) -> const function* { return pair.second.get(); }) |
         std::ranges::to<std::vector>();
}

const function* function_store::emplace_builtin(std::string&& name,
                                                ptr_type ret,
                                                const std::vector<ptr_type>& params,
                                                builtin_function::descriptor_t&& desc,
                                                bool inline_) {
  auto ptr =
      std::make_unique<builtin_function>(std::move(name), ret, params, std::move(desc), inline_);
  auto mangled = mangled_name::function(name, params);
  return m_store.emplace(mangled.str(), std::move(ptr)).first->second.get();
}
const function* function_store::emplace_user_provided(const ast::decl::function* decl,
                                                      bool inline_) {

  auto mang =
      function::signature_t(decl->ident.value(), decl->parameters.transform([](const auto& v) {
        return &v.specifiers.parse_type();
      })).mangle();
  return m_store
      .emplace(mang.str(),
               std::make_unique<user_function>(
                   decl, decl->specifiers.parse_linkage(), decl->specifiers.parse_type(), inline_))
      .first->second.get();
}

void function_store::clear() { m_store.clear(); }

bool conversion_store::is_convertible(const type& from, const type& to) const {
  auto mangled_from = from.format();
  bool res          = false;
  if (const auto& outer = m_direct_store.at(mangled_from); m_direct_store.contains(mangled_from)) {
    if (outer.contains(to.format())) {
      return true;
    }
  }

  return std::ranges::any_of(m_glob_store, [&to](const glob_conversion_function& fn) -> bool {
    return fn.is_convertible(to);
  });
}

std::vector<ptr_type> conversion_store::get_convertible_types(cr_type from) const {
  return get_conversions(from) | std::views::transform([&from](const auto& conversion) -> ptr_type {
           return &conversion->to(from);
         }) |
         std::ranges::to<std::vector>();
}

std::vector<const conversion_function*> conversion_store::get_conversions(cr_type from) const {
  auto mangled_from = from.format();
  auto glob_range =
      m_glob_store | std::views::filter([&from](const glob_conversion_function& fn) -> bool {
        return fn.is_convertible(from);
      }) |
      std::views::transform([](const glob_conversion_function& fn) -> const conversion_function* {
        return static_cast<const conversion_function*>(&fn);
      }) |
      std::ranges::to<std::vector>();
  if (m_direct_store.contains(mangled_from)) {
    auto range = m_direct_store.at(mangled_from) | std::views::values |
                 std::views::transform(
                     [](const direct_conversion_function& ptr) -> const conversion_function* {
                       return &ptr;
                     }) |
                 std::ranges::to<std::vector>();
    glob_range.insert(glob_range.end(), range.begin(), range.end());
  }
  return glob_range;
}

void conversion_store::emplace_direct(conversion_function::body_t body, cr_type f, cr_type t) {
  auto& value = m_direct_store[f.format()];
  value.emplace(t.format(), direct_conversion_function(std::move(body), f, t));
}

void conversion_store::emplace_glob(std::string desc,
                                    conversion_function::body_t body,
                                    glob_conversion_function::condition_t c,
                                    glob_conversion_function::extractor_t e) {
  m_glob_store.emplace_back(std::move(desc), std::move(body), std::move(c), std::move(e));
}

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
    return scope_.variables.contains(ident.value());
  });
}

[[nodiscard]] variable* frame::get(const ast::term::identifier& ident) {
  if (!is_declared(ident)) {
    throw_error<_error_t::UNDECLARED_SYMBOL>(ident);
  }

  for (auto& scope : scopes) {
    if (scope.variables.contains(ident.value())) {
      return scope.variables.get(ident.value());
    }
  }
  UNREACHABLE();
}

[[nodiscard]] const variable* frame::get(const ast::term::identifier& ident) const {
  if (!is_declared(ident)) {
    throw_error<_error_t::UNDECLARED_SYMBOL>(ident);
  }

  for (const auto& scope : scopes) {
    if (scope.variables.contains(ident.value())) {
      return scope.variables.get(ident.value());
    }
  }

  UNREACHABLE("Unrecheable");
}

void symbol_table::push_frame(const user_function* fn) {
  m_stackframe.emplace_back(fn);
  active_frame().create_scope(*fn->body);
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
  return &active_frame().labels.at(ident.value());
}

const variable* symbol_table::get_variable(id ident) const {
  if (m_global_scope.variables.contains(ident.value())) {
    return m_global_scope.variables.get(ident.value());
  }

  return active_frame().get(ident);
}

std::optional<const function*> symbol_table::progressive_prefix_match(
    const std::vector<ptr_type>& argument_types,
    const std::vector<const function*>& possible_fns) const {
  auto range =
      argument_types | std::views::enumerate |
      std::views::transform([this, &possible_fns](const auto& pair) {
        auto i                 = std::get<0>(pair);
        ptr_type castable_type = std::get<1>(pair);

        auto r                 = m_conversions.get_convertible_types(*castable_type) |
                 std::views::transform([this, i, &possible_fns](ptr_type casted_type) {
                   auto p = possible_fns |
                            std::views::filter([this, i, casted_type](const function* fn) {
                              return fn->argument_types().at(i)->format() == casted_type->format();
                            });
                   return p;
                 }) |
                 std::views::join | std::ranges::to<std::vector>();
        return r;
      }) |
      std::views::join | std::ranges::to<std::vector>();
  if (range.size() == 1) {
    return range[0];
  }

  return std::nullopt; // no unique match found
}

const function* symbol_table::get_function(const function::signature_t& sig) const {
  if (const auto* candidate = m_functions.get(sig); candidate != nullptr) {
    return candidate;
  }
  auto f            = m_conversions.get_convertible_types(*sig.argument_types.at(0));
  auto candidates   = m_functions.get_by_name(sig.name);
  auto filtered_map = candidates | std::views::filter([&sig](const auto& fn) {
                        return fn->argument_types().size() == sig.argument_types.size();
                      }) |
                      std::ranges::to<std::vector>();
  if (!filtered_map.empty()) {
    auto opt = progressive_prefix_match(sig.argument_types, filtered_map);
    if (opt.has_value()) {
      return opt.value();
    }
  }

  throw_error<_error_t::UNDECLARED_SYMBOL>(ast::term::identifier(sig.name));
}

void symbol_table::declare_function(const ast::decl::function* func, bool inline_) {
  REGISTER_TRACE("Creating func {}", func->ident);
  m_functions.emplace_user_provided(func, inline_);
}

user_function* symbol_table::get_entry_point() { return m_entry_point.get(); }

void symbol_table::link_entry_point(const ast::decl::function* fn) {
  if (!is_entry_point_defined()) {
    m_entry_point = std::make_unique<user_function>(
        fn, fn->specifiers.parse_linkage(), fn->specifiers.parse_type(), true);
  } else {
    throw_error<_error_t::ALREADY_DECLARED_SYMBOL>(*fn);
  }
}

bool symbol_table::is_global_scope() const noexcept { return m_stackframe.empty(); }

bool symbol_table::in_main() const noexcept {
  return m_stackframe.top().func.get().identifier() == "main";
}

std::optional<operand*> compilation_unit::call_builtin(
    const builtin::function::builtin_signature_t& header,
    const std::vector<operand*>& args) {
  const auto* fn = table.get_function(header.signature());
  return fn->run(*this, args);
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
  reserve_memory(decl.ident->format(), "resq", "1");
  auto* addr = table.m_global_scope.create_user_provided(
      &decl, operand_factory::instance().create<label_memory>(decl.ident->value()));
  move(addr, init);
  return addr->variable();
}

const variable* compilation_unit::declare_local_variable(const ast::decl::variable& decl,
                                                         operand* init) {
  // + 1 because not counted yet
  const auto* var = table.active_frame()
                        .active_scope()
                        .create_user_provided(&decl,
                                              operand_factory::instance().create<stack_memory>(
                                                  table.active_frame().local_stack.size() + 1))
                        ->variable();
  push(init);
  return var;
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
  asmgen.add_section_data(assembly::asmgen::Section::BS, name, kw, times);
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
  auto* reg = regs.parameters.at(0);
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

  /* if (m_state.current_phase != Phase::EXITING) { */
  // If does not have changed the phase,
  // it means that it hasnt exited or returned
  if (current_phase != Phase::EXITING) {
    exit_successfully();
  }

  asmgen.write_label("exit");
  syscall("60");

  REGISTER_TRACE("Code generation finished");
  return asmgen.end();
}

static_assert(std::formattable<operand*, char>);

} // namespace cmm::ir
