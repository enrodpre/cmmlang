#include "ast.hpp"
#include "asm.hpp"
#include "common.hpp"
#include "expr.h"
#include "ir.hpp"
#include "lang.hpp"
#include "types.hpp"
#include "visitor.hpp"
#include <algorithm>
#include <ranges>
#include <type_traits>
#include <utility>
#include <utils.hpp>

namespace cmm::ast {

using namespace decl;

label::label(const token& label_)
    : visitable(label_) {
  add(ident);
}

specifiers::specifiers(ast::type&& t, ast::linkage&& l, ast::storage&& s)
    : type(std::move(t)),
      linkage(std::move(l)),
      storage(std::move(s)) {
  add_all(&type, &linkage, &storage);
}

decl::rank::rank(const token& l, decltype(number) e, const token& r)
    : open(l),
      number(e),
      close(r) {
  add_all(&open, number, &close);
}
decl::rank::rank(const token& l, const token& r)
    : rank(r, nullptr, l) {}

variable::variable(specifiers&& spec, decltype(rank) r, identifier&& id, decltype(init) i)
    : specs(std::move(spec)),
      rank(r),
      visitable(std::move(id)),
      init(i) {
  add_all(&specs, &ident, init);
}
variable::variable(cr_type t, decltype(ident)&& id, decltype(init) init)
    : variable(specifiers(t), nullptr, std::move(id), init) {}
variable::variable(cr_type t, std::string s)
    : variable(t, identifier(std::move(s)), nullptr) {}

function::function(decltype(specs)&& s,
                   decltype(ident)&& ident_,
                   decltype(params)&& args,
                   decltype(body) body_)
    : specs(std::move(s)),
      visitable(std::move(ident_)),
      params(std::move(args)),
      body(body_) {
  add_all(&specs, &ident, &params, body);
  // a specs.set_parent(this);
  // ident.set_parent(this);
  // params.set_parent(this);
  // if (body != nullptr) {
  //   body->set_parent(this);
  // }
}

assembly::operand* decl::conversion_function::operator()(assembly::operand* reg) const noexcept {
  cr_type from = reg->content_type();
  ASSERT(is_convertible(from));
  cr_type to_t = to(from);

  // TODO
  return {};
  // return run(compilation_unidecl::t::instance());
}

void function::parameters::load_arguments(const siblings<expr::expression*>& args) {
  for (int i = 0; i < vector<parameter>::size(); ++i) {
    auto& param              = vector<parameter>::at(i);
    expr::expression* n_expr = param->init;
    if (i < args.size()) {
      n_expr = args.at(i);
    }
    if (n_expr != nullptr) {
      throw_error<compilation_error_t::BAD_FUNCTION_CALL>(*param);
    }
    param->init = n_expr;
  }
}

selection::if_::if_(const token& t,
                    decltype(condition) condition,
                    decltype(block) block_,
                    decltype(else_) else_)
    : keyword(t),
      condition(condition),
      block(block_),
      else_(else_) {
  add_all(&keyword, &condition, block, else_);
}

namespace {
  std::string condition_label() { return std::format("cond_{}", "it"); }
  std::string exit_label() { return std::format("exit_{}", "it"); }
} // namespace
iteration::while_::while_(const token& t, expr::expression& condition_, block* block)
    : keyword(t),
      condition(condition_),
      body(block) {
  add_all(&keyword, &condition, body);
}

iteration::for_::for_(const token& t,
                      decl::variable* start_,
                      expr::expression* condition_,
                      expr::expression* step_,
                      block* block)
    : keyword(t),
      start(start_),
      condition(condition_),
      step(step_),
      body(block) {
  add_all(&keyword, start, condition, step, body);
}

jump::goto_::goto_(const token& token)
    : term(token) {
  add(&term);
}

jump::break_::break_(const token& token)
    : keyword(token) {
  add(&keyword);
}
jump::continue_::continue_(const token& token)
    : keyword(token) {
  add(&keyword);
}
jump::return_::return_(ast::keyword&& k, expr::expression* expr_)
    : keyword(std::move(k)),
      expr(expr_) {
  add(&keyword);
}

// std::vector<ptr_type> conversion_store::get_convertible_types(cr_type from) const {
//   return get_conversions(from) | std::views::transform([&from](const auto& conversion) ->
//   ptr_type {
//            return &conversion->to(from);
//          }) |
//          std::ranges::to<std::vector>();
// }

// std::vector<const conversion_function*> conversion_store::get_conversions(cr_type from) const {
//   auto mangled_from = from.format();
//   auto glob_range =
//       m_glob_store | std::views::filter([&from](const glob_conversion& fn) -> bool {
//         return fn.is_convertible(from);
//       }) |
//       std::views::transform([](const glob_conversion& fn) -> const conversion_function* {
//         return static_cast<const conversion_function*>(&fn);
//       }) |
//       std::ranges::to<std::vector>();
//   if (m_direct_store.contains(mangled_from)) {
//     auto range =
//         m_direct_store.at(mangled_from) | std::views::values |
//         std::views::transform(
//             [](const direct_conversion& ptr) -> const conversion_function* { return &ptr; }) |
//         std::ranges::to<std::vector>();
//     glob_range.insert(glob_range.end(), range.begin(), range.end());
//   return glob_range;
// }

signature ast::decl::function::sig() const { return {ident, params.types()}; }

void decl::function::definition::clear() noexcept {
  while (!local_scopes.empty()) {
    destroy_scope();
  }
  local_scopes.clear();
}

[[nodiscard]] bool decl::function::definition::is_declared(const ast::identifier& ident) const {
  return variables.contains(ident) ||
         std::ranges::any_of(local_scopes, [&ident](const scope* scope) {
           return scope->is_declared(ident.value());
         });
}
std::vector<function_store::value_type> function_store::get_by_name(cstring name) const {
  return data() | std::views::filter([name](const auto& pair) -> bool {
           const auto& [k, v] = pair;
           return k == name;
         }) |
         std::views::transform([name](const auto& pair) { return pair.second; }) |
         std::ranges::to<std::vector>();
}

void function_store::insert(function* v) { hashmap::insert(v->sig(), v); }

[[nodiscard]] bool scope::is_declared(const ast::identifier& id) const {
  return variables.contains(id);
}

[[nodiscard]] variable* scope::get_variable(const ast::identifier& ident) {
  return variables.at(ident);
}
[[nodiscard]] const variable* scope::get_variable(const ast::identifier& ident) const {
  return variables.at(ident);
}

void variable_store::put(decl::variable* var) { insert(var->string(), var); }

[[nodiscard]] variable* decl::function::definition::get_variable(const ast::identifier& ident) {
  if (!is_declared(ident)) {
    throw_error<compilation_error_t::UNDECLARED_SYMBOL>(ident);
  }

  if (variables.contains(ident)) {
    return variables.at(ident);
  }

  for (const auto& scope : local_scopes) {
    if (scope->variables.contains(ident.value())) {
      return scope->variables.at(ident.value());
    }
  }

  UNREACHABLE("Unrecheable");
}
[[nodiscard]] const variable* decl::function::definition::get_variable(
    const ast::identifier& ident) const {
  if (!is_declared(ident)) {
    throw_error<compilation_error_t::UNDECLARED_SYMBOL>(ident);
  }

  if (variables.contains(ident)) {
    return variables.at(ident);
  }

  for (const auto& scope : local_scopes) {
    if (scope->variables.contains(ident.value())) {
      return scope->variables.at(ident.value());
    }
  }

  UNREACHABLE("Unrecheable");
}

assembly::operand* function::definition::declare_parameter(ast::decl::variable* var,
                                                           assembly::operand* op) {
  var->address = op;
  variables.put(var);
  return op->hold_value(var);
}
size_t translation_unit::pop_frame() {
  DEBUG_ASSERT(!is_global_scope());
  size_t ditched = 0;
  for (int i = 0; i < active_frame()->local_scopes.size(); ++i) {
    ditched += m_stackframe.top()->destroy_scope();
  }
  m_stackframe.pop();
  return ditched;
};

void decl::function::definition::create_scope(ast::block& comp) noexcept {
  local_scopes.push(&comp);
}

size_t decl::function::definition::destroy_scope() noexcept {
  size_t ditched = local_scopes.top()->variables.size();
  DEBUG_ASSERT(!local_scopes.empty());
  local_scopes.pop();
  REGISTER_TRACE("Cleared local scope: ditched {} elements", ditched);
  return ditched;
}

void translation_unit::clear() noexcept {
  m_stackframe.clear();
  m_functions.clear();
  variables.clear();
}

bool translation_unit::is_entry_point_defined() const noexcept {
  builtin_signature_data a = builtin_signature_t::MAIN;
  return m_functions.contains(a.signature());
  // std::string translation_unit::format() const {
  // return "";
  // m_global_scope.variables.join(", ");
  //}
}

const decl::label* translation_unit::get_label(const ast::identifier& ident) const {
  return active_frame()->labels.at(ident.value());
}

void block::declare_label(const ast::decl::label* l) { labels.emplace(l->ident.value(), l); }

void scope::declare_variable(ast::decl::variable* decl) {
  variables.insert(decl->ident.value(), decl);
}

void translation_unit::create_frame(decl::function::definition* fn) {
  m_stackframe.push(fn);
  m_stackframe.top()->create_scope(*fn);
}

variable* translation_unit::get_variable(const ast::identifier& ident) {
  if (variables.contains(ident.value())) {
    return variables.at(ident.value());
  }

  return active_frame()->get_variable(ident);
}

const variable* translation_unit::get_variable(const ast::identifier& ident) const {
  if (variables.contains(ident.value())) {
    return variables.at(ident.value());
  }

  return active_frame()->get_variable(ident);
}
void translation_unit::declare_variable(ast::decl::variable* var) {
  if (m_stackframe.empty()) {
    auto* op     = assembly::operand_factory::create_label(var->string());
    var->address = op;
    variables.put(var);
    op->hold_address(var);
    m_context->reserve_static_var(var->string());
  } else {
    auto offset  = active_frame()->local_stack.size() + 1;
    auto* addr   = assembly::operand_factory::create_stack_memory(offset);
    var->address = addr;
    active_frame()->declare_variable(var);
    addr->hold_value(var);
  }
}

std::optional<function*> translation_unit::progressive_prefix_match(
    const std::vector<ptr_type>& argument_types,
    const std::vector<function*>& possible_fns) const {
  auto range =
      argument_types | std::views::enumerate |
      std::views::transform([this, &possible_fns](const auto& pair) {
        auto i                 = std::get<0>(pair);
        ptr_type castable_type = std::get<1>(pair);

        auto r                 = conversions::get_convertible_types(castable_type) |
                 std::views::transform([this, i, &possible_fns](ptr_type casted_type) {
                   auto p = possible_fns |
                            std::views::filter([this, i, casted_type](const function* fn) {
                              return fn->params.types().at(i)->format() == casted_type->format();
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

function* translation_unit::get_function(const signature& sig) {
  const auto& [name, args_types] = sig;
  if (m_functions.contains(sig)) {
    return m_functions.at(sig);
  }
  auto f                            = conversions::get_convertible_types(args_types.at(0));
  std::vector<function*> candidates = m_functions.get_by_name(name.value());
  auto opt                          = progressive_prefix_match(args_types, candidates);
  if (opt.has_value()) {
    return opt.value();
  }

  throw_error<compilation_error_t::UNDECLARED_SYMBOL>(name);
}

const function* translation_unit::get_function(const signature& sig) const {
  const auto& [name, args_types] = sig;
  if (const auto* candidate = m_functions.at(sig); candidate != nullptr) {
    return candidate;
  }
  auto f          = conversions::get_convertible_types(args_types.at(0));
  auto candidates = m_functions.get_by_name(name.value());
  auto opt        = progressive_prefix_match(args_types, candidates);
  if (opt.has_value()) {
    return opt.value();
  }

  throw_error<compilation_error_t::UNDECLARED_SYMBOL>(name);
}

void translation_unit::declare_function(decl::function* func, bool) {
  if (is_declared<decl::function>(func->ident)) {
    throw_error<compilation_error_t::ALREADY_DECLARED_SYMBOL>(func->ident);
  }
  REGISTER_TRACE("Creating func {}", func->ident.value());
  m_functions.insert(func);
}

const function* translation_unit::get_entry_point() {
  builtin_signature_data a = builtin_signature_t::MAIN;
  return m_functions.at(a.signature());
}

void translation_unit::link_entry_point(ast::decl::function* fn) {
  if (!is_entry_point_defined()) {
    builtin_signature_data main_header = builtin_signature_t::MAIN;
    m_functions.insert(fn);
  } else {
    throw_error<compilation_error_t::ALREADY_DECLARED_SYMBOL>(*fn);
  }
}

[[nodiscard]] scope* translation_unit::active_scope() noexcept {
  if (m_stackframe.empty()) {
    return this;
  }
  return m_stackframe.top();
}
[[nodiscard]] const scope* translation_unit::active_scope() const noexcept {
  if (m_stackframe.empty()) {
    return this;
  }
  return m_stackframe.top();
}
bool translation_unit::is_global_scope() const noexcept { return m_stackframe.empty(); }

bool translation_unit::in_main() const noexcept {
  const auto* parent = m_stackframe.top()->get_parent<ast::decl::function>();
  return parent->ident.value() == "main";
}

template <typename T>
bool translation_unit::is_declarable(const ast::identifier&) const noexcept {
  if constexpr (std::is_same_v<decl::variable, T>) {
    // Only check current scope
    return !active_scope()->variables.contains(ident);
  } else if constexpr (std::is_same_v<decl::function, T>) {
    return m_functions.get_by_name(ident.value()).size() > 0;
  } else {
    return active_frame()->labels.contains(ident.value());
  }
}

template bool translation_unit::is_declarable<decl::function>(
    const ast::identifier&) const noexcept;
template bool translation_unit::is_declarable<decl::variable>(
    const ast::identifier&) const noexcept;
template bool translation_unit::is_declarable<decl::label>(const ast::identifier&) const noexcept;

template <typename T>
bool translation_unit::is_declared(const ast::identifier&) const noexcept {
  if constexpr (std::is_same_v<decl::variable, T>) {
    // Only check current scope

    return (!is_global_scope() && !m_stackframe.empty() && active_frame()->is_declared(ident)) ||
           variables.contains(ident.value());
  } else if constexpr (std::is_same_v<decl::function, T>) {
    return m_functions.get_by_name(ident.value()).size() > 0;
  } else {
    return active_frame()->labels.contains(ident.value());
  }
}

void translation_unit::set_context(ir::compilation_unit* ctx) { m_context = ctx; }
template bool translation_unit::is_declared<decl::label>(const ast::identifier&) const noexcept;

void ast_visitor::visit(ast::literal& c) { TRACE_VISITOR(c); }
void ast_visitor::visit(ast::keyword& c) { TRACE_VISITOR(c); }
void ast_visitor::visit(ast::identifier& c) { TRACE_VISITOR(c); }
void ast_visitor::visit(ast::decl::function::parameters& p) {
  TRACE_VISITOR(p);
  for (auto&& par : p) {
    par->accept(*this);
  }
};
void ast_visitor::visit(ast::decl::specifiers& c) {
  TRACE_VISITOR(c);
  c.storage.accept(*this);
  c.linkage.accept(*this);
  c.type.accept(*this);
};
void ast_visitor::visit(ast ::expr ::identifier& c) { TRACE_VISITOR(c); };
void ast_visitor::visit(ast ::expr ::unary_operator& c) {
  TRACE_VISITOR(c);
  c.operator_.accept(*this);
  c.expr.accept(*this);
};
void ast_visitor::visit(ast ::expr ::binary_operator& c) {
  TRACE_VISITOR(c);
  c.operator_.accept(*this);
  c.left.accept(*this);
  c.right.accept(*this);
};
void ast_visitor::visit(ast ::expr ::call& c) {
  TRACE_VISITOR(c);
  c.ident.accept(*this);
  c.args.accept(*this);
};
void ast_visitor::visit(ast ::decl ::variable& c) {
  TRACE_VISITOR(c);
  c.ident.accept(*this);
  c.specs.accept(*this);
  if (auto* init = c.init) {
    init->accept(*this);
  }
};
void ast_visitor::visit(ast ::decl ::function& c) {
  TRACE_VISITOR(c);
  c.specs.accept(*this);
  c.ident.accept(*this);
  c.params.accept(*this);
  if (auto* body = c.body) {
    body->accept(*this);
  }
};
void ast_visitor::visit(ast ::decl ::label& c) {
  TRACE_VISITOR(c);
  c.ident.accept(*this);
};
void ast_visitor::visit(ast ::iteration ::while_& c) {
  TRACE_VISITOR(c);
  c.condition.accept(*this);
  if (auto* body = c.body) {
    body->accept(*this);
  }
};
void ast_visitor::visit(ast ::iteration ::for_& c) {
  TRACE_VISITOR(c);
  c.start->accept(*this);
  if (auto* cond = c.condition) {
    cond->accept(*this);
  }
  if (auto* step = c.step) {
    step->accept(*this);
  }
  if (auto* body = c.body) {
    body->accept(*this);
  }
};
void ast_visitor::visit(ast ::selection ::if_& c) {
  TRACE_VISITOR(c);
  c.condition.accept(*this);
  if (auto* block = c.block) {
    block->accept(*this);
  }
  if (auto* else_ = c.else_) {
    else_->accept(*this);
  }
};
void ast_visitor::visit(ast ::jump ::goto_& c) {
  TRACE_VISITOR(c);
  c.term.accept(*this);
};
void ast_visitor::visit(ast ::jump ::return_& c) {
  TRACE_VISITOR(c);
  if (auto* expr = c.expr) {
    expr->accept(*this);
  }
}
void ast_visitor::visit(ast::jump::continue_& c) { TRACE_VISITOR(c); }
void ast_visitor::visit(ast::expr::literal& c) { TRACE_VISITOR(c); }
void ast_visitor::visit(ast::jump::break_& c) { TRACE_VISITOR(c); }
void ast_visitor::visit(ast::operator_& c) { TRACE_VISITOR(c); }
void ast_visitor::visit(ast::storage& c) { TRACE_VISITOR(c); }
void ast_visitor::visit(ast::type& c) { TRACE_VISITOR(c); }
void ast_visitor::visit(ast::linkage& c) { TRACE_VISITOR(c); }
void ast_visitor::visit(siblings<expr::expression*>& c) { TRACE_VISITOR(c); }
void ast_visitor::visit(ast ::expr ::implicit_type_conversion& c) { TRACE_VISITOR(c); }
void ast_visitor::visit(ast::decl::function::definition& c) {
  TRACE_VISITOR(c);
  for (const auto& stmt : c.stmts) {
    stmt->accept(*this);
  }
}
void ast_visitor::visit(ast::decl::block& c) {
  TRACE_VISITOR(c);
  for (const auto& stmt : c.stmts) {
    stmt->accept(*this);
  }
}
void const_ast_visitor::visit(const ast::operator_& c) { TRACE_VISITOR(c); }
void const_ast_visitor::visit(const ast::literal& c) { TRACE_VISITOR(c); }
void const_ast_visitor::visit(const ast::keyword& c) { TRACE_VISITOR(c); }
void const_ast_visitor::visit(const ast::identifier& c) { TRACE_VISITOR(c); }
void const_ast_visitor::visit(const siblings<expr::expression*>& c) { TRACE_VISITOR(c); }
void const_ast_visitor::visit(const ast ::expr ::implicit_type_conversion& c) { TRACE_VISITOR(c); }
void const_ast_visitor::visit(const ast::decl::function::parameters& p) {
  TRACE_VISITOR(p);
  for (auto&& par : p) {
    par->accept(*this);
  }
};
void const_ast_visitor::visit(const ast::decl::specifiers& s) {
  TRACE_VISITOR(s);
  s.accept(*this);
};
void const_ast_visitor::visit(const ast ::expr ::identifier& c) { TRACE_VISITOR(c); };
void const_ast_visitor::visit(const ast ::expr ::unary_operator& c) {
  TRACE_VISITOR(c);
  c.operator_.accept(*this);
  c.expr.accept(*this);
};
void const_ast_visitor::visit(const ast ::expr ::binary_operator& c) {
  TRACE_VISITOR(c);
  c.operator_.accept(*this);
  c.left.accept(*this);
  c.right.accept(*this);
};
void const_ast_visitor::visit(const ast ::expr ::call& c) {
  TRACE_VISITOR(c);
  c.ident.accept(*this);
  c.args.accept(*this);
};
void const_ast_visitor::visit(const ast ::decl ::variable& c) {
  TRACE_VISITOR(c);
  c.ident.accept(*this);
  c.specs.accept(*this);
  if (auto* init = c.init) {
    init->accept(*this);
  }
};
void const_ast_visitor::visit(const ast ::decl ::function& c) {
  TRACE_VISITOR(c);
  c.specs.accept(*this);
  c.ident.accept(*this);
  c.params.accept(*this);
  if (auto* body = c.body) {
    body->accept(*this);
  }
};
void const_ast_visitor::visit(const ast ::decl ::label& c) {
  TRACE_VISITOR(c);
  c.ident.accept(*this);
};
void const_ast_visitor::visit(const ast ::iteration ::while_& c) {
  TRACE_VISITOR(c);
  c.condition.accept(*this);
  if (auto* body = c.body) {
    body->accept(*this);
  }
};
void const_ast_visitor::visit(const ast ::iteration ::for_& c) {
  TRACE_VISITOR(c);
  c.start->accept(*this);
  if (auto* cond = c.condition) {
    cond->accept(*this);
  }
  if (auto* step = c.step) {
    step->accept(*this);
  }
  if (auto* body = c.body) {
    body->accept(*this);
  }
};
void const_ast_visitor::visit(const ast ::selection ::if_& c) {
  TRACE_VISITOR(c);
  c.condition.accept(*this);
  if (auto* block = c.block) {
    block->accept(*this);
  }
  if (auto* else_ = c.else_) {
    else_->accept(*this);
  }
};
void const_ast_visitor::visit(const ast ::jump ::goto_& c) {
  TRACE_VISITOR(c);
  c.term.accept(*this);
};
void const_ast_visitor::visit(const ast ::jump ::return_& c) {
  TRACE_VISITOR(c);
  if (auto* expr = c.expr) {
    expr->accept(*this);
  }
}
void const_ast_visitor::visit(const ast::jump::continue_& c) { TRACE_VISITOR(c); }
void const_ast_visitor::visit(const ast::jump::break_& c) { TRACE_VISITOR(c); }
void const_ast_visitor::visit(const ast::storage& c) { TRACE_VISITOR(c); }
void const_ast_visitor::visit(const ast::linkage& c) { TRACE_VISITOR(c); }
void const_ast_visitor::visit(const ast::type& c) { TRACE_VISITOR(c); }
void const_ast_visitor::visit(const ast::expr::literal& c) { TRACE_VISITOR(c); }
void const_ast_visitor::visit(const ast::decl::function::definition& c) {
  TRACE_VISITOR(c);
  for (const auto& stmt : c.stmts) {
    stmt->accept(*this);
  }
}
void const_ast_visitor::visit(const ast::decl::block& c) {
  TRACE_VISITOR(c);
  for (const auto& stmt : c.stmts) {
    stmt->accept(*this);
  }
}
} // namespace cmm::ast
