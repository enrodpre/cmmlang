#include "ast.hpp"
#include "asm.hpp"
#include "common.hpp"
#include "expr.h"
#include "lang.hpp"
#include "types.hpp"
#include <ranges>
#include <type_traits>
#include <utility>
#include <utils.hpp>

namespace cmm::ast {

// std ::string expr ::call ::arguments ::repr(size_t n) const {
//   return std ::format("{}{}",
//                       std ::string(2, ' '),
//                       std ::format("{}({}):\n{}",
//                                    "Arguments",
//                                    vector<expr::expression*>::size(),
//                                    vector<expr::expression*>::join('\n', n)));
// }
// std ::string expr ::call ::arguments ::format() const {
//   return cpptrace ::demangle(typeid(this).name());
// }
// JOIN_INDENT_IMPL(decl::function::parameters, "Parameters", '\n')
// FORMAT_INDENT_IMPL(decl::specifiers, "Specifiers:\n{}\n{}\n{}", type, linkage, storage)
// FORMAT_INDENT_IMPL(expr::identifier, "Identifier:\n{}", (*this));
// FORMAT_INDENT_IMPL(expr::literal, "Literal:\n{}", (*this));
// FORMAT_INDENT_IMPL(expr::unary_operator, "Unary:\n{}\n{}", operator_, expr);
// FORMAT_INDENT_IMPL(expr::binary_operator, "Binary:\n{}\n{}\n{}\n", operator_, left, right);
// FORMAT_INDENT_IMPL(expr::call, "Call:\n{}\n{}\n", ident, args);
// FORMAT_INDENT_IMPL(decl::label, "Label:\n{}", ident);
// FORMAT_INDENT_IMPL(decl::variable, "Variable:\n{}", specs);
// std ::string decl ::function ::repr(size_t n) const {
//   auto params_str = params.vector<parameter>::transform(
//                         [n](const decl::function::parameter& param) -> std::string {
//                           return param.repr(n + 1);
//                         }) |
//                     std::views::join_with('\n') | std::ranges::to<std::string>();
//   auto elems = body->vector<statement*>::transform(
//                    [n](const statement* stmt) -> std::string { return stmt->repr(n + 1); }) |
//                std::views::join_with('\n') | std::ranges::to<std::string>();
//   auto res =
//       std::format("{}{}",
//                   std::string(n * 2, ' '),
//                   std::format("Function:\n{}\n{}\n{}", ident.repr(n + 1), params_str, elems));
//   return res;
// };
// std::string decl::function::format() const { return cpptrace::demangle(typeid(this).name()); }
// std ::string iteration ::while_ ::repr(size_t n) const {
//   return std ::format(
//       "{}{}", std ::string(n * 2, ' '), std ::format("While:\n{}", condition.repr(n + 1)));
// }
// std ::string iteration ::while_ ::format() const { return "while"; };
// std ::string iteration ::for_ ::repr(size_t n) const {
//   return std ::format(
//       "{}{}", std ::string(n * 2, ' '), std ::format("For:\n{}", (*condition).repr(n + 1)));
// }
// std ::string iteration ::for_ ::format() const { return "for"; };
// std ::string selection ::if_ ::repr(size_t n) const {
//   return std ::format("{}{}",
//                       std ::string(n * 2, ' '),
//                       std ::format("If:\n{}\n{}\n", condition.repr(n + 1), (*block).repr(n +
//                       1)));
// }
// std ::string selection ::if_ ::format() const { return "if"; };
// INDENT_IMPL(jump::break_, "{}", "Break");
// INDENT_IMPL(jump::continue_, "{}", "Continue");
// FORMAT_INDENT_IMPL(jump::return_, "Return:\n{}", (*expr));
// FORMAT_INDENT_IMPL(jump::goto_, "Goto({})", term);

template <typename T>
[[nodiscard]] std::vector<const node*> identifiable<T>::path() const {
  std::vector<const node*> result;
  const auto* child = static_cast<const T*>(this);
  if (child == nullptr) {
    return result;
  }

  const node* current = child->get_parent();
  while (current != nullptr) {
    result.push_back(current);
    current = current->get_parent();
  }
  return result;
}

leaf::leaf(cmm::location loc)
    : m_location(std::move(loc)) {}

leaf::leaf(const token& t)
    : m_location(t.location()) {}
terms::literal::literal(cmm::location l, std::string s)
    : visitable(std::move(l)),
      m_value(std::move(s)) {}
using namespace decl;

label::label(const token& label_)
    : visitable(label_) {
  ident.set_parent(this);
}

specifiers::specifiers(terms::type t, terms::linkage l, terms::storage s)
    : type(std::move(t)),
      linkage(std::move(l)),
      storage(std::move(s)) {}

variable::variable(specifiers&& spec, terms::identifier&& id, decltype(init) i)
    : specs(std::move(spec)),
      visitable(std::move(id)),
      init(i) {
  specs.set_parent(this);
  if (init != nullptr) {
    init->set_parent(this);
  }
}
variable::variable(cr_type t, decltype(ident)&& id, decltype(init) init)
    : variable(specifiers(t), std::move(id), init) {}
variable::variable(cr_type t, std::string s)
    : variable(t, terms::identifier(std::move(s)), nullptr) {}
cmm::location decl::label::location() const { return ident.location(); }
cmm::location decl::variable::location() const {
  return specs.location() + ident.location() + GET_LOC(init);
}

function::function(decltype(specs)&& s,
                   decltype(ident)&& ident_,
                   decltype(params)&& args,
                   decltype(body) body_)
    : specs(std::move(s)),
      visitable(std::move(ident_)),
      params(std::move(args)),
      body(body_) {
  specs.set_parent(this);
  ident.set_parent(this);
  params.set_parent(this);
  if (body != nullptr) {
    body->local_scope::set_parent(this);
  }
}

// decl::conversion_function::conversion_function(const decltype(body)& body)
//     : function(nullptr, body, false),
//       type(conversion_type_t::IMPLICIT),
//       body(body) {}

assembly::operand* decl::conversion_function::operator()(assembly::operand* reg) const noexcept {
  cr_type from = reg->content_type();
  ASSERT(is_convertible(from));
  cr_type to_t = to(from);

  // return run(compilation_unidecl::t::instance());
}
decl::direct_conversion::direct_conversion(const decltype(body)& body, cr_type from, cr_type to)
    : conversion_function(body),
      from_type(from),
      to_type(to) {}
decl::glob_conversion::glob_conversion(std::string desc,
                                       const decltype(body)& body,
                                       const condition_t& from,
                                       const extractor_t& to)
    : conversion_function(body),
      description(std::move(desc)),
      condition(from),
      extractor(to) {}

void function::parameters::load_arguments(const ast::expr::arguments& args) {
  for (int i = 0; i < vector<parameter>::size(); ++i) {
    auto& param              = vector<parameter>::at(i);
    expr::expression* n_expr = param.init;
    if (i < args.vector<expr::expression*>::size()) {
      n_expr = args.vector<expr::expression*>::at(i);
    }
    if (n_expr != nullptr) {
      throw_error<error_t::BAD_FUNCTION_CALL>(param);
    }
    param.init = n_expr;
  }
}

cmm::location decl::function::location() const {
  return specs.location() + ident.location() + params.location() +
         (body == nullptr ? location() : body->local_scope::location());
}

selection::if_::if_(terms::keyword&& k,
                    decltype(condition) condition,
                    decltype(block) block_,
                    decltype(else_) else_)
    : keyword(std::move(k)),
      condition(condition),
      block(block_),
      else_(else_) {
  keyword.set_parent(this);
  condition.set_parent(this);
  if (block != nullptr) {
    block->set_parent(this);
  }
  if (else_ != nullptr) {
    else_->set_parent(this);
  }
}

cmm::location selection::if_::location() const {
  return keyword.location() + condition.location() + GET_LOC(else_) + GET_LOC(block);
}

template <typename It>
std::string iteration::iteration<It>::condition_label() const {
  return std::format("cond_{}", typeid(It).name());
}
template <typename It>
std::string iteration::iteration<It>::exit_label() const {
  return std::format("exit_{}", typeid(It).name());
}

iteration::while_::while_(terms::keyword&& k, expr::expression& condition_, statement* block)
    : keyword(std::move(k)),
      condition(condition_),
      body(block) {
  keyword.set_parent(this);
  condition.set_parent(this);
  if (body != nullptr) {
    body->set_parent(this);
  }
}

cmm::location iteration::while_::location() const {
  return keyword.location() + condition.location() + GET_LOC(body);
}

iteration::for_::for_(terms::keyword&& k,
                      decl::variable* start_,
                      expr::expression* condition_,
                      expr::expression* step_,
                      statement* block)
    : keyword(std::move(k)),
      start(start_),
      condition(condition_),
      step(step_),
      body(block) {
  keyword.set_parent(this);
  if (condition != nullptr) {
    condition->set_parent(this);
  }
  if (body != nullptr) {
    body->set_parent(this);
  }
  if (start != nullptr) {
    start->set_parent(this);
  }
  if (step != nullptr) {
    step->set_parent(this);
  }
}

cmm::location iteration::for_::location() const {
  return keyword.location() + GET_LOC(condition) + GET_LOC(body) + GET_LOC(start) + GET_LOC(step);
}

jump::goto_::goto_(const token& token)
    : term(token) {
  term.set_parent(this);
}
cmm::location jump::goto_::location() const { return term.location(); }

jump::break_::break_(const token& token)
    : keyword(token) {
  keyword.set_parent(this);
}
cmm::location jump::break_::location() const { return keyword.location(); }
jump::continue_::continue_(const token& token)
    : keyword(token) {
  keyword.set_parent(this);
}
cmm::location jump::continue_::location() const { return keyword.location(); }
jump::return_::return_(terms::keyword k, expr::expression* expr_)
    : keyword(std::move(k)),
      expr(expr_) {
  keyword.set_parent(this);
}
cmm::location jump::return_::location() const { return keyword.location() + GET_LOC(expr); }

bool conversion_store::is_convertible(const type& from, const type& to) const {
  auto mangled_from = from.format();
  bool res          = false;
  if (const auto& outer = m_direct_store.at(mangled_from); m_direct_store.contains(mangled_from)) {
    if (outer->contains(to.format())) {
      return true;
    }
  }

  return std::ranges::any_of(
      m_glob_store, [&to](const glob_conversion& fn) -> bool { return fn.is_convertible(to); });
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
      m_glob_store | std::views::filter([&from](const glob_conversion& fn) -> bool {
        return fn.is_convertible(from);
      }) |
      std::views::transform([](const glob_conversion& fn) -> const conversion_function* {
        return static_cast<const conversion_function*>(&fn);
      }) |
      std::ranges::to<std::vector>();
  if (m_direct_store.contains(mangled_from)) {
    auto range =
        m_direct_store.at(mangled_from) | std::views::values |
        std::views::transform(
            [](const direct_conversion& ptr) -> const conversion_function* { return &ptr; }) |
        std::ranges::to<std::vector>();
    glob_range.insert(glob_range.end(), range.begin(), range.end());
  }
  return glob_range;
}

void conversion_store::emplace_direct(const decltype(conversion_function::body)& body,
                                      cr_type f,
                                      cr_type t) {
  const auto* value = m_direct_store[f.format()];
  value->emplace(t.format(), direct_conversion(body, f, t));
}

void conversion_store::emplace_glob(std::string&& desc,
                                    const decltype(conversion_function::body)& body,
                                    const glob_conversion::condition_t& c,
                                    const glob_conversion::extractor_t& e) {
  m_glob_store.emplace_back(std::move(desc), body, c, e);
}

frame::frame(const ast::decl::function* fn)
    : function(fn) {
  create_scope(fn);
}

void frame::clear() noexcept {
  while (!local_scopes.empty()) {
    destroy_scope();
  }
  local_scopes.clear();
}

local_scope& frame::active_scope() {
  DEBUG_ASSERT(!local_scopes.empty());
  return local_scopes.top();
}
[[nodiscard]] const local_scope& frame::active_scope() const {
  DEBUG_ASSERT(!local_scopes.empty());
  return local_scopes.top();
}

[[nodiscard]] bool frame::is_declared(const ast::terms::identifier& ident) const noexcept {
  return std::ranges::any_of(
      local_scopes.cbegin(), local_scopes.cend(), [ident](const local_scope& scope_) {
        return scope_.variables.contains(ident.value());
      });
}

[[nodiscard]] variable* frame::get(const ast::terms::identifier& ident) {
  if (!is_declared(ident)) {
    throw_error<_error_t::UNDECLARED_SYMBOL>(ident);
  }

  for (auto& scope : local_scopes) {
    if (scope.variables.contains(ident.value())) {
      return scope.variables.at(ident.value());
    }
  }
  UNREACHABLE();
}

using translation_unit = scopes::translation_unit;

[[nodiscard]] const variable* frame::get(const ast::terms::identifier& ident) const {
  if (!is_declared(ident)) {
    throw_error<_error_t::UNDECLARED_SYMBOL>(ident);
  }

  for (const auto& scope : local_scopes) {
    if (scope.variables.contains(ident.value())) {
      return scope.variables.at(ident.value());
    }
  }

  UNREACHABLE("Unrecheable");
}

static_assert(std::is_class_v<frame>);

size_t translation_unit::pop_frame() {
  DEBUG_ASSERT(!is_global_scope());
  size_t ditched = 0;
  for (int i = 0; i < active_frame().local_scopes.size(); ++i) {
    ditched += active_frame().destroy_scope();
  }
  m_stackframe.pop();
  return ditched;
};

void frame::create_scope(const ast::scopes::function_scope& comp) noexcept {
  local_scopes.emplace_back(std::move(comp));
}

size_t frame::destroy_scope() noexcept {
  size_t ditched = local_scopes.top().variables.size();
  DEBUG_ASSERT(!local_scopes.empty());
  local_scopes.pop();
  REGISTER_TRACE("Cleared local scope: ditched {} elements", ditched);
  return ditched;
}

void translation_unit::clear() noexcept {
  m_stackframe.clear();
  m_functions.clear();
  m_global_scope.variables.clear();
}

[[nodiscard]] frame& translation_unit::active_frame() noexcept {
  DEBUG_ASSERT(!m_stackframe.empty());
  return m_stackframe.top();
}

[[nodiscard]] const frame& translation_unit::active_frame() const noexcept {
  DEBUG_ASSERT(!m_stackframe.empty());
  return m_stackframe.top();
}

bool translation_unit::is_entry_point_defined() const noexcept {
  builtin_signature_t a = builtin_signature_t::MAIN;
  return m_functions.at(a.mangle()) != nullptr;
}
// std::string translation_unit::format() const {
// return "";
// m_global_scope.variables.join(", ");
//}

const decl::label* translation_unit::get_label(const ast::terms::identifier& ident) const {
  return active_frame().labels.at(ident.value());
}

const variable* translation_unit::get_variable(const ast::terms::identifier& ident) const {
  if (m_global_scope.variables.contains(ident.value())) {
    return m_global_scope.variables.at(ident.value());
  }

  return active_frame().get(ident);
}

std::optional<const function*> translation_unit::progressive_prefix_match(
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

const function* translation_unit::get_function(const function_signature& sig) const {
  const auto& [name, args_types] = sig;
  if (const auto* candidate = m_functions.at(name); candidate != nullptr) {
    return candidate;
  }
  auto f            = m_conversions.get_convertible_types(args_types.at(0)->specs.type);
  auto candidates   = m_functions.get_by_name(name);
  auto filtered_map = candidates | std::views::filter([&sig, &args_types](const auto& fn) {
                        return fn->params.size() == args_types.size();
                      }) |
                      std::ranges::to<std::vector>();
  if (!filtered_map.empty()) {
    auto opt = progressive_prefix_match(args_types, filtered_map);
    if (opt.has_value()) {
      return opt.value();
    }
  }

  throw_error<_error_t::UNDECLARED_SYMBOL>(ast::terms::identifier(name));
}

void translation_unit::declare_function(const ast::decl::function* func, bool inline_) {
  REGISTER_TRACE("Creating func {}", func->ident.value());
  m_functions.emplace_user_provided(func, inline_);
}

const function* translation_unit::get_entry_point() {
  builtin_signature_t a = builtin_signature_t::MAIN;
  return m_functions.at(a.MAIN);
}

void translation_unit::link_entry_point(const ast::decl::function* fn) {
  if (!is_entry_point_defined()) {
    m_entry_point = std::make_unique<function>(fn, fn->body, true);
  } else {
    throw_error<_error_t::ALREADY_DECLARED_SYMBOL>(*fn);
  }
}

bool translation_unit::is_global_scope() const noexcept { return m_stackframe.empty(); }

bool translation_unit::in_main() const noexcept {
  const auto* parent = m_stackframe.top().function->get_parent();
  const auto* fn     = dynamic_cast<const ast::decl::function*>(parent);
  return fn->ident.value() == "main";
}
scopes::block::block(block&& v) noexcept
    : visitable(std::move(v)) {
  local_scope::set_parent(static_cast<local_scope*>(this));
}
template <typename T>
bool translation_unit::is_declarable(const ast::terms::identifier&) const noexcept {
  if constexpr (std::is_same_v<ir::variable, T>) {
    // Only check current scope
    return !active_scope().variables.contains(ident.value());
  } else if constexpr (std::is_same_v<ir::function, T>) {
    return m_functions.get_by_name(ident.value()).size() > 0;
  } else {
    return active_frame().labels.contains(ident.value());
  }
}

template <typename T>
bool translation_unit::is_declared(symbol_table::identifier_type ident) const noexcept {
  if constexpr (std::is_same_v<ir::variable, T>) {
    // Only check current scope

    return (!is_global_scope() && !m_stackframe.empty() && active_frame().is_declared(ident)) ||
           m_global_scope.variables.contains(ident.value());
  } else if constexpr (std::is_same_v<ir::function, T>) {
    return m_functions.get_by_name(ident.value()).size() > 0;
  } else {
    return active_frame().labels.contains(ident.value());
  }
}
void visit(ast::term::operator_& c) override { TRACE_VISITOR(c); }
void visit(ast::term::literal& c) override { TRACE_VISITOR(c); }
void visit(ast::term::keyword& c) override { TRACE_VISITOR(c); }
void visit(ast::term::specifier& c) override { TRACE_VISITOR(c); }
void visit(ast::term::identifier& c) override { TRACE_VISITOR(c); }
void visit(ast::expr::call::arguments& a) override {
  TRACE_VISITOR(a);
  for (auto&& arg : a) {
    arg->accept(*this);
  }
};
void visit(ast::decl::function::parameters_t& p) override {
  TRACE_VISITOR(p);
  for (auto&& par : p) {
    par.accept(*this);
  }
};
void visit(ast::decl::specifiers& s) override {
  TRACE_VISITOR(s);
  for (auto&& par : s) {
    par.accept(*this);
  }
};
void visit(ast ::expr ::identifier& c) override {
  TRACE_VISITOR(c);
  c.term.accept(*this);
};
void visit(ast ::expr ::literal& c) override {
  TRACE_VISITOR(c);
  c.term.accept(*this);
};
void visit(ast ::expr ::unary_operator& c) override {
  TRACE_VISITOR(c);
  c.operator_.accept(*this);
  c.expr.accept(*this);
};
void visit(ast ::expr ::binary_operator& c) override {
  TRACE_VISITOR(c);
  c.operator_.accept(*this);
  c.left.accept(*this);
  c.right.accept(*this);
};
void visit(ast ::expr ::call& c) override {
  TRACE_VISITOR(c);
  c.ident.accept(*this);
  c.args.accept(*this);
};
void visit(ast ::compound& c) override {
  TRACE_VISITOR(c);
  for (auto&& d : c) {
    d->accept(*this);
  }
};
void visit(ast ::decl ::variable& c) override {
  TRACE_VISITOR(c);
  if (auto* ident = c.ident) {
    ident->accept(*this);
  }
  c.specifiers.accept(*this);
  if (auto* init = c.init) {
    init->accept(*this);
  }
};
void visit(ast ::decl ::function& c) override {
  TRACE_VISITOR(c);
  c.specifiers.accept(*this);
  c.ident.accept(*this);
  c.parameters.accept(*this);
  if (auto* body = c.body) {
    body->accept(*this);
  }
};
void visit(ast ::decl ::label& c) override {
  TRACE_VISITOR(c);
  c.term.accept(*this);
};
void visit(ast ::iteration ::while_& c) override {
  TRACE_VISITOR(c);
  c.condition.accept(*this);
  if (auto* body = c.body) {
    body->accept(*this);
  }
};
void visit(ast ::iteration ::for_& c) override {
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
void visit(ast ::selection ::if_& c) override {
  TRACE_VISITOR(c);
  c.condition.accept(*this);
  if (auto* block = c.block) {
    block->accept(*this);
  }
  if (auto* else_ = c.else_) {
    else_->accept(*this);
  }
};
void visit(ast ::jump ::goto_& c) override {
  TRACE_VISITOR(c);
  c.term.accept(*this);
};
void visit(ast ::jump ::return_& c) override {
  TRACE_VISITOR(c);
  if (auto* expr = c.expr) {
    expr->accept(*this);
  }
}
void visit(ast::jump::continue_& c) override { TRACE_VISITOR(c); }
void visit(ast::jump::break_& c) override { TRACE_VISITOR(c); }
void visit(ast::program& p) override {
  TRACE_VISITOR(p);
  for (auto&& d : p) {
    p.accept(*this);
  }
}
void visit(const ast::term::operator_& c) override { TRACE_VISITOR(c); }
void visit(const ast::term::literal& c) override { TRACE_VISITOR(c); }
void visit(const ast::term::keyword& c) override { TRACE_VISITOR(c); }
void visit(const ast::term::specifier& c) override { TRACE_VISITOR(c); }
void visit(const ast::term::identifier& c) override { TRACE_VISITOR(c); }
void visit(const ast::expr::call::arguments& a) override {
  TRACE_VISITOR(a);
  for (auto&& arg : a) {
    arg->accept(*this);
  }
};
void visit(const ast::decl::function::parameters_t& p) override {
  TRACE_VISITOR(p);
  for (auto&& par : p) {
    par.accept(*this);
  }
};
void visit(const ast::decl::specifiers& s) override {
  TRACE_VISITOR(s);
  for (auto&& par : s) {
    par.accept(*this);
  }
};
void visit(const ast ::expr ::identifier& c) override {
  TRACE_VISITOR(c);
  c.term.accept(*this);
};
void visit(const ast ::expr ::literal& c) override {
  TRACE_VISITOR(c);
  c.term.accept(*this);
};
void visit(const ast ::expr ::unary_operator& c) override {
  TRACE_VISITOR(c);
  c.operator_.accept(*this);
  c.expr.accept(*this);
};
void visit(const ast ::expr ::binary_operator& c) override {
  TRACE_VISITOR(c);
  c.operator_.accept(*this);
  c.left.accept(*this);
  c.right.accept(*this);
};
void visit(const ast ::expr ::call& c) override {
  TRACE_VISITOR(c);
  c.ident.accept(*this);
  c.args.accept(*this);
};
void visit(const ast ::compound& c) override {
  TRACE_VISITOR(c);
  for (auto&& d : c) {
    d->accept(*this);
  }
};
void visit(const ast ::decl ::variable& c) override {
  TRACE_VISITOR(c);
  if (auto* ident = c.ident) {
    ident->accept(*this);
  }
  c.specifiers.accept(*this);
  if (auto* init = c.init) {
    init->accept(*this);
  }
};
void visit(const ast ::decl ::function& c) override {
  TRACE_VISITOR(c);
  c.specifiers.accept(*this);
  c.ident.accept(*this);
  c.parameters.accept(*this);
  if (auto* body = c.body) {
    body->accept(*this);
  }
};
void visit(const ast ::decl ::label& c) override {
  TRACE_VISITOR(c);
  c.term.accept(*this);
};
void visit(const ast ::iteration ::while_& c) override {
  TRACE_VISITOR(c);
  c.condition.accept(*this);
  if (auto* body = c.body) {
    body->accept(*this);
  }
};
void visit(const ast ::iteration ::for_& c) override {
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
void visit(const ast ::selection ::if_& c) override {
  TRACE_VISITOR(c);
  c.condition.accept(*this);
  if (auto* block = c.block) {
    block->accept(*this);
  }
  if (auto* else_ = c.else_) {
    else_->accept(*this);
  }
};
void visit(const ast ::jump ::goto_& c) override {
  TRACE_VISITOR(c);
  c.term.accept(*this);
};
void visit(const ast ::jump ::return_& c) override {
  TRACE_VISITOR(c);
  if (auto* expr = c.expr) {
    expr->accept(*this);
  }
}
void visit(const ast::jump::continue_& c) override { TRACE_VISITOR(c); }
void visit(const ast::jump::break_& c) override { TRACE_VISITOR(c); }
void visit(const ast::program& p) override {
  TRACE_VISITOR(p);
  for (auto&& d : p) {
    p.accept(*this);
  }
} // namespace cmm::ast
