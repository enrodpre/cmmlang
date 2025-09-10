#include "ast.hpp"
#include "asm.hpp"
#include "common.hpp"
#include "expr.h"
#include "ir.hpp"
#include "lang.hpp"
#include "types.hpp"
#include <algorithm>
#include <initializer_list>
#include <ranges>
#include <type_traits>
#include <utility>

namespace cmm::ast {

using namespace decl;

template <typename... Ts>
std::vector<node*> lift_to_nodes(Ts*... ptrs) {
  return {static_cast<node*>(ptrs)...};
}
template <typename... Args>
std::vector<node*> concat_nodes(Args&&... args) {
  return lift_to_nodes(std::forward<Args>(args)...);
}
label::label(const token& label_)
    : derived_visitable(label_) {}

specifiers::specifiers(ast::type_spec&& t, ast::linkage_spec&& l, ast::storage_spec&& s)
    : type(std::move(t)),
      linkage(std::move(l)),
      storage(std::move(s)) {}

rank::rank(const token& l, decltype(number) e, const token& r)
    : open(l),
      number(e),
      close(r) {}

std::string rank::string() const { return std::format("[{}]", number->string()); }

decl::rank::rank(const token& l, const token& r)
    : rank(r, nullptr, l) {}

std::vector<node*> decl::rank::children() {
  return {dynamic_cast<node*>(&open), dynamic_cast<node*>(number), dynamic_cast<node*>(&close)};
}

variable::variable(specifiers&& spec, decltype(rank) r, identifier id, decltype(init) i)
    : derived_visitable<variable, declaration>(std::move(id)),
      specs(std::move(spec)),
      rank(r),
      init(i) {}

variable::variable(types::type_id t, decltype(ident) id, decltype(init) init)
    : variable(specifiers(std::move(t)), nullptr, std::move(id), init) {}

std::string variable::string() const {
  return std::format("{} {}{} = {}", specs, *rank, ident, *init);
}

std::vector<node*> variable::children() { return concat_nodes(&specs, rank, init); }

function::function(decltype(specs)&& s,
                   const token& ident_,
                   decltype(params)&& args,
                   decltype(body) body_)
    : derived_visitable(ast::identifier{ident_.location(),
                                        mangle_function(std::string(ident_.value),
                                                        args | TRANSFORM([](const variable& v) {
                                                          return v.specs.type.value()->string();
                                                        }),
                                                        '_')}),
      specs(std::move(s)),
      params(std::move(args)),
      body(body_) {}

std::vector<node*> decl::function::children() {
  return {to_node(&ident), to_node(&specs), to_node(&params), to_node(body)};
}

std::string decl::function::repr() const { return std::format("{} {}", specs, ident); }
std::string decl::function::string() const { return std::format("{}", ident); }

assembly::operand* decl::conversion_function::operator()(assembly::operand* reg) const noexcept {
  // auto from = reg->content_type();

  // TODO
  return {};
  // return run(compilation_unidecl::t::instance());
}

selection::if_::if_(const token& t, decltype(condition) c, decltype(block) b, decltype(else_) e)
    : keyword(t, keyword_t::IF),
      condition(c),
      block(b),
      else_(e) {}

std::vector<node*> selection::if_::children() {
  return concat_nodes(&keyword, &condition, block, else_);
}

iteration::while_::while_(const token& t, expr::expression& condition_, block* block)
    : keyword(t, keyword_t::WHILE),
      condition(condition_),
      body(block) {}

std::vector<node*> iteration::while_::children() {
  return concat_nodes(&keyword, &condition, body);
}

iteration::for_::for_(const token& t,
                      decl::variable* start_,
                      expr::expression* condition_,
                      expr::expression* step_,
                      block* block)
    : keyword(t, keyword_t::FOR),
      start(start_),
      condition(condition_),
      step(step_),
      body(block) {}

std::vector<node*> iteration::for_::children() {
  return concat_nodes(&keyword, start, condition, step, body);
}

jump::goto_::goto_(const token& token)
    : term(token.location(), std::string(token.value)) {}
std::vector<node*> jump::goto_::children() { return {&term}; }

jump::break_::break_(const token& token)
    : keyword(token, keyword_t::BREAK) {}
std::vector<node*> jump::break_::children() { return {&keyword}; }

jump::continue_::continue_(const token& token)
    : keyword(token, keyword_t::CONTINUE) {}
std::vector<node*> jump::continue_::children() { return {&keyword}; }

jump::return_::return_(const token& t_token, expr::expression* expr_)
    : keyword(t_token, keyword_t::RETURN),
      expr(expr_) {}
std::vector<node*> jump::return_::children() { return concat_nodes(&keyword, expr); }

// std::vector<type> conversion_store::get_convertible_types(type from) const {
//   return get_conversions(from) | std::views::transform([&from](const auto& conversion) ->
//   type {
//            return &conversion->to(from);
//          }) |
//          std::ranges::to<std::vector>();
// }

// std::vector<const conversion_function*> conversion_store::get_conversions(type from) const {
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

void decl::function::definition::clear() noexcept {
  while (!local_scopes.empty()) {
    destroy_scope();
  }
  local_scopes.clear();
}

[[nodiscard]] bool decl::function::definition::is_declared(const ast::identifier& id) const {
  return variables.contains(id) || std::ranges::any_of(local_scopes, [id](const scope* s) {
           return s->variables.contains(id.value().data());
         });
}

std::vector<function_store::value_type> function_store::get_by_name(const std::string& name) const {
  return data() | std::views::filter([name](const auto& pair) -> bool {
           std::string key = pair.first;
           if (size_t pos = key.find_first_of('_')) {
             return name == key.substr(0, pos);
           }
           return key == name;
         }) |
         std::views::transform([](const auto& pair) { return pair.second; }) |
         std::ranges::to<std::vector>();
}

void function_store::insert(function* v) { hashmap::insert(v->ident.string(), v); }

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
  hashmap::insert(var->ident.string(), std::make_pair(var, op));
}

[[nodiscard]] const variable_store::value_type& decl::function::definition::get_variable(
    const ast::identifier& ident) const {
  if (variables.contains(ident)) {
    return variables.at(ident);
  }

  for (const auto& s : local_scopes) {
    if (s->variables.contains(ident.value().data())) {
      return s->variables.at(ident.value().data());
    }
  }

  THROW(UNDECLARED_SYMBOL, ident);
}

assembly::operand* function::definition::declare_parameter(const parameter& var,
                                                           assembly::operand* op) {
  variables.insert(&var, op);
  return op->hold_value(&var);
}

size_t translation_unit::pop_frame() {
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
  local_scopes.pop();
  REGISTER_TRACE("Cleared local scope: ditched {} elements", ditched);
  return ditched;
}

namespace {
template <cmm::instruction_t Ins>
cmm::instruction single_ins(cmm::arg_t l = cmm::arg_t::LEFT, cmm::arg_t r = cmm::arg_t::RIGHT) {
  return cmm::instruction{.ins = Ins, .arg1 = l, .arg2 = r};
} // namespace

template <instruction_t Ins>
std::vector<instruction> create_ins(arg_t l = arg_t::LEFT, arg_t r = arg_t::RIGHT) {
  return {
      instruction{.ins = Ins, .arg1 = l, .arg2 = r}
  };
}
template <instruction_t Ins>
constexpr builtin_callable create_same_args(const type_id& args) {
  return {args, args, args, create_ins<Ins>()};
}

template <instruction_t Ins>
constexpr builtin_callable create_same_args(type_id ret, const type_id& args) {
  return {std::move(ret), args, args, create_ins<Ins>()};
}

constexpr auto create_overloaded_op(const std::vector<type_id>& types, std::vector<instruction> ins)
    -> std::vector<std::tuple<type_id, std::vector<type_id>, std::vector<instruction>>> {
  return types | std::views::transform([ins](type_id t) {
           return std::make_tuple(t, std::vector<type_id>{t, t}, ins);
         }) |
         TO_VEC;
}

template <instruction_t Ins>
constexpr auto create_overloaded_op(type_id ret, const std::vector<type_id>& types)
    -> std::vector<std::tuple<type_id, std::vector<type_id>, std::vector<instruction>>> {

  return types | std::views::transform([ret](type_id t) {
           return std::make_tuple(ret, std::vector<type_id>{t, t}, create_ins<Ins>());
         }) |
         TO_VEC;
}

constexpr auto provide_operator_overloads(
    operator_t op,
    std::vector<std::tuple<type_id, std::vector<type_id>, std::vector<instruction>>> ovs)
    -> std::pair<operator_t,
                 std::vector<std::tuple<type_id, std::vector<type_id>, std::vector<instruction>>>> {
  return std::make_pair(op, ovs);
}

using builtin_data = std::vector<
    std::pair<operator_t,
              std::vector<std::tuple<type_id, std::vector<type_id>, std::vector<instruction>>>>>;
auto provide_builtin_callables() {
  builtin_data data = {
      {
       provide_operator_overloads(
              operator_t::assign,
       {{SINTREF_T, {SINTREF_T, SINT_T}, create_ins<instruction_t::mov>()},
               {UINTREF_T, {UINTREF_T, UINT_T}, create_ins<instruction_t::mov>()},
               {FLOATREF_T, {FLOATREF_T, FLOAT_T}, create_ins<instruction_t::mov>()}}),

       // Unary
          provide_operator_overloads(operator_t::pre_inc,
       {{SINTREF_T, {SINTREF_T}, create_ins<instruction_t::inc>()}}),
       provide_operator_overloads(operator_t::pre_dec,
       {{SINTREF_T, {SINTREF_T}, create_ins<instruction_t::dec>()}}),
       provide_operator_overloads(
              operator_t::post_inc,
       {{SINT_T, {SINTREF_T, SINT_T}, create_ins<instruction_t::inc>()}}),
       provide_operator_overloads(
              operator_t::post_dec,
       {{SINT_T, {SINTREF_T, SINT_T}, create_ins<instruction_t::dec>()}}),
       // Comparisons
          provide_operator_overloads(operator_t::eq,
       create_overloaded_op<instruction_t::mov>(
                                         BOOL_T, {BOOL_T, SINT_T, UINT_T, FLOAT_T, CHAR_T})),
       provide_operator_overloads(operator_t::neq,
       create_overloaded_op<instruction_t::mov>(
                                         BOOL_T, {BOOL_T, SINT_T, UINT_T, FLOAT_T, CHAR_T})),
       provide_operator_overloads(operator_t::gt,
       create_overloaded_op<instruction_t::mov>(
                                         BOOL_T, {BOOL_T, SINT_T, UINT_T, FLOAT_T, CHAR_T})),
       provide_operator_overloads(operator_t::ge,
       create_overloaded_op<instruction_t::mov>(
                                         BOOL_T, {BOOL_T, SINT_T, UINT_T, FLOAT_T, CHAR_T})),
       provide_operator_overloads(operator_t::lt,
       create_overloaded_op<instruction_t::mov>(
                                         BOOL_T, {BOOL_T, SINT_T, UINT_T, FLOAT_T, CHAR_T})),
       provide_operator_overloads(operator_t::le,
       create_overloaded_op<instruction_t::mov>(
                                         BOOL_T, {BOOL_T, SINT_T, UINT_T, FLOAT_T, CHAR_T})),
       // Arithmetic
          provide_operator_overloads(operator_t::plus,
       create_overloaded_op({BOOL_T, SINT_T, UINT_T, FLOAT_T},
       create_ins<instruction_t::add>())),
       provide_operator_overloads(
              operator_t::minus,
       create_overloaded_op({SINT_T, UINT_T, FLOAT_T}, create_ins<instruction_t::sub>())),
       provide_operator_overloads(
              operator_t::star,
       create_overloaded_op({SINT_T, UINT_T, FLOAT_T},
       {single_ins<instruction_t::mov>(arg_t::ACC, arg_t::LEFT),
                                    single_ins<instruction_t::mul>(arg_t::RIGHT),
                                    single_ins<instruction_t::mov>(arg_t::LEFT, arg_t::ACC)})),
       provide_operator_overloads(
              operator_t::fslash,
       create_overloaded_op({SINT_T, UINT_T, FLOAT_T},
       {single_ins<instruction_t::mov>(arg_t::ACC, arg_t::LEFT),
                                    single_ins<instruction_t::mul>(arg_t::RIGHT),
                                    single_ins<instruction_t::mov>(arg_t::LEFT, arg_t::ACC)})),
       // {operator_t::or_, {{unary_matchers::any, {}, {}, create_ins<instruction_t::or_>()}}},
          // {operator_t::xor_, {{unary_matchers::any, {}, {},
          // create_ins<instruction_t::xor_>()}}}, {operator_t::and_, {{unary_matchers::any, {},
          // {}, create_ins<instruction_t::and_>()}}}, {operator_t::not_, {unary_matchers::any,
          // {}, VOID_T, create_ins<instruction_t::not_>()}}}
      }
  };
  return data | std::views::transform([](const auto& pair) {
           operator_t op = pair.first;
           std::vector<std::tuple<type_id, std::vector<type_id>, std::vector<instruction>>>
               overloads = pair.second;
           auto callables =
               overloads |
               std::views::transform(
                   [op](const std::tuple<type_id, std::vector<type_id>, std::vector<instruction>>&
                            data) -> builtin_callable {
                     const auto& [ret, types, ins] = data;
                     return {op, ret, types, ins};
                   }) |
               TO_VEC;
           return std::make_pair(op, callables);
         }) |
         std::ranges::to<std::unordered_map>();
}
} // namespace
translation_unit::translation_unit()
    : builtin_operators(provide_builtin_callables()) {}

void translation_unit::clear() noexcept {
  m_stackframe.clear();
  m_functions.clear();
  variables.clear();
}

const decl::label* translation_unit::get_label(const ast::identifier& id) const {
  return active_frame()->labels.at(id.value().data());
}

void block::declare_label(const ast::decl::label* l) { labels.emplace(l->ident.value(), l); }

assembly::operand* scope::declare_variable(ast::decl::variable* decl) {
  variables.insert(decl, nullptr);
  return nullptr;
}

void translation_unit::create_frame(decl::function::definition* fn) { m_stackframe.push(fn); }

const variable_store::value_type& translation_unit::get_variable(const ast::identifier& id) const {
  if (variables.contains(id.value().data())) {
    return variables.at(id.value().data());
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

void translation_unit::declare_function(decl::function* func, bool) {
  if (is_declared<decl::function>(func->ident)) {
    THROW(ALREADY_DECLARED_SYMBOL, func->ident);
  }
  REGISTER_TRACE("Creating func {}", func->ident.value());
  m_functions.insert(func);
}

void translation_unit::define_function(ast::decl::function* t_func) {
  // if (ast->is_declar) {
  //   throw_error<compilation_error_t::ALREADY_DEFINED_FUNCTION>(t_func->ident);
  // }

  create_frame(t_func->body);
  auto registers = m_context->regs.parameters();
  for (const variable& param : t_func->parameters()) {
    active_frame()->declare_parameter(param, registers.next());
  }
  active_frame()->local_stack.push();
  m_context->asmgen.load_new_procedure(t_func->ident.value());
  m_context->runner.generate_statements(*t_func->body);

  m_context->asmgen.save_current_procedure();

  size_t ditched_vars = pop_frame();

  if (!is_global_scope()) {
    active_frame()->local_stack.pop(ditched_vars + 1);
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
    return !m_functions.contains(id.string());
  } else {
    return !active_frame()->labels.contains(id);
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

std::vector<const ast::decl::function::definition*> translation_unit::stackframe() {
  std::vector<const ast::decl::function::definition*> res;
  while (!m_stackframe.empty()) {
    res.push_back(std::move(m_stackframe.top()));
    m_stackframe.pop();
  }
  return res;
}
void translation_unit::set_context(ir::compilation_unit* ctx) { m_context = ctx; }

template bool translation_unit::is_declared<decl::label>(const ast::identifier&) const noexcept;

} // namespace cmm::ast
