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

leaf::leaf(std::optional<cmm::location> loc)
    : m_location(std::move(loc)) {}

leaf::leaf(const token& t)
    : m_location(t.location()) {}

label::label(const token& label_)
    : visitable<label, declaration>(label_) {
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

variable::variable(specifiers&& spec, decltype(rank) r, identifier id, decltype(init) i)
    : visitable<variable, declaration>(std::move(id)),
      specs(std::move(spec)),
      rank(r),
      init(i) {
  add_all(&specs, &ident, init);
}

variable::variable(ptype t, decltype(ident) id, decltype(init) init)
    : variable(specifiers(std::move(t)), nullptr, std::move(id), init) {}

function::function(decltype(specs)&& s,
                   decltype(ident)&& ident_,
                   decltype(params)&& args,
                   decltype(body) body_)
    : visitable<function, declaration>(std::move(ident_)),
      specs(std::move(s)),
      params(std::move(args)),
      body(body_) {
  add_all(&specs, &ident, &params, body);
}

assembly::operand* decl::conversion_function::operator()(assembly::operand* reg) const noexcept {
  crptype from = reg->content_type();
  ASSERT(is_convertible(from));
  crptype to_t = to(from);

  // TODO
  return {};
  // return run(compilation_unidecl::t::instance());
}

selection::if_::if_(const token& t, decltype(condition) c, decltype(block) b, decltype(else_) e)
    : keyword(t),
      condition(c),
      block(b),
      else_(e) {
  add_all(&keyword, &condition, block, else_);
}

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

// std::vector<ptype> conversion_store::get_convertible_types(crtype from) const {
//   return get_conversions(from) | std::views::transform([&from](const auto& conversion) ->
//   ptype {
//            return &conversion->to(from);
//          }) |
//          std::ranges::to<std::vector>();
// }

// std::vector<const conversion_function*> conversion_store::get_conversions(crtype from) const {
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

callable_contract ast::decl::function::contract() const {
  return {specs.type.value(),
          ident.value(),
          params | std::views::transform([](const auto& param) -> ptype {
            return param.specs.type.value();
          }) | std::ranges::to<std::vector>()};
}

void decl::function::definition::clear() noexcept {
  while (!local_scopes.empty()) {
    destroy_scope();
  }
  local_scopes.clear();
}

[[nodiscard]] bool decl::function::definition::is_declared(const ast::identifier& id) const {
  return variables.contains(id) || std::ranges::any_of(local_scopes, [id](const scope* s) {
           return s->variables.contains(id.value());
         });
}

std::vector<function_store::value_type> function_store::get_by_name(const std::string& name) const {
  return data() | std::views::filter([name](const auto& pair) -> bool {
           const auto& [k, v] = pair;
           return k == name;
         }) |
         std::views::transform([](const auto& pair) { return pair.second; }) |
         std::ranges::to<std::vector>();
}

void function_store::insert(function* v) { hashmap::insert(v->signature(), v); }

[[nodiscard]] bool scope::is_declared(const ast::identifier& id) const {
  return variables.contains(id);
}

[[nodiscard]] const variable_store::value_type& scope::get_variable(
    const ast::identifier& ident) const {
  return variables.at(ident);
}
[[nodiscard]] const variable* scope::get_variable_declaration(const ast::identifier& id) const {
  return get_variable(id).first;
}
[[nodiscard]] assembly::operand* scope::get_variable_address(const ast::identifier& id) const {
  return get_variable(id).second;
}

void variable_store::insert(const decl::variable* var, assembly::operand* op) {
  hashmap::insert(var->string(), std::make_pair(var, op));
}

[[nodiscard]] const variable_store::value_type& decl::function::definition::get_variable(
    const ast::identifier& ident) const {
  if (variables.contains(ident)) {
    return variables.at(ident);
  }

  for (const auto& s : local_scopes) {
    if (s->variables.contains(ident.value())) {
      return s->variables.at(ident.value());
    }
  }

  THROW(UNDECLARED_SYMBOL, ident);
}

assembly::operand* function::definition::declare_parameter(const ast::decl::variable& var,
                                                           assembly::operand* op) {
  variables.insert(&var, op);
  return op->hold_value(&var);
}

size_t translation_unit::pop_frame() {
  DEBUG_ASSERT(!is_global_scope());
  size_t ditched = 0;
  for (size_t i = 0; i < active_frame()->local_scopes.size(); ++i) {
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

const decl::label* translation_unit::get_label(const ast::identifier& id) const {
  return active_frame()->labels.at(id.value());
}

void block::declare_label(const ast::decl::label* l) { labels.emplace(l->ident.value(), l); }

assembly::operand* scope::declare_variable(ast::decl::variable* decl) {
  variables.insert(decl, nullptr);
  NOT_IMPLEMENTED;
}

void translation_unit::create_frame(decl::function::definition* fn) {
  m_stackframe.push(fn);
  // m_stackframe.top()->create_scope(*fn);
}

const variable_store::value_type& translation_unit::get_variable(const ast::identifier& id) const {
  if (variables.contains(id.value())) {
    return variables.at(id.value());
  }

  return active_frame()->get_variable(id);
}

assembly::operand* translation_unit::declare_variable(ast::decl::variable* var) {
  assembly::operand* addr = nullptr;
  if (m_stackframe.empty()) {
    addr = assembly::operand_factory::create_label(var->string());
    variables.insert(var, addr);
    addr->hold_address(var);
    m_context->reserve_static_var(var->string());
  } else {
    auto offset = active_frame()->local_stack.size() + 1;
    addr        = assembly::operand_factory::create_stack_memory(offset);
    active_frame()->variables.insert(var, addr);
    addr->hold_value(var);
  }
  return addr;
}

std::vector<const function*> translation_unit::user_defined_functions(
    const ast::identifier& id) const {
  return m_functions.get_by_name(id);
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
bool translation_unit::is_declarable(const ast::identifier& id) const noexcept {
  if constexpr (std::is_same_v<decl::variable, T>) {
    // Only check current scope
    return !active_scope()->variables.contains(id);
  } else if constexpr (std::is_same_v<decl::function, T>) {
    return m_functions.get_by_name(id).size() > 0;
  } else {
    return active_frame()->labels.contains(id);
  }
}

template bool translation_unit::is_declarable<decl::function>(
    const ast::identifier&) const noexcept;
template bool translation_unit::is_declarable<decl::variable>(
    const ast::identifier&) const noexcept;
template bool translation_unit::is_declarable<decl::label>(const ast::identifier&) const noexcept;

template <typename T>
bool translation_unit::is_declared(const ast::identifier& id) const noexcept {
  if constexpr (std::is_same_v<decl::variable, T>) {
    // Only check current scope

    return (!is_global_scope() && !m_stackframe.empty() && active_frame()->is_declared(id)) ||
           variables.contains(id);
  } else if constexpr (std::is_same_v<decl::function, T>) {
    return m_functions.get_by_name(id).size() > 0;
  } else {
    return active_frame()->labels.contains(id);
  }
}

void translation_unit::set_context(ir::compilation_unit* ctx) { m_context = ctx; }

template bool translation_unit::is_declared<decl::label>(const ast::identifier&) const noexcept;

void ast_visitor::visit(ast::literal& c) { TRACE_VISITOR(c); }

void ast_visitor::visit(ast::keyword& c) { TRACE_VISITOR(c); }

void ast_visitor::visit(ast::identifier& c) { TRACE_VISITOR(c); }

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
  for (auto& param : c.params) {
    param.accept(*this);
  }
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
  for (const auto& param : c.params) {
    param.accept(*this);
  }
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
