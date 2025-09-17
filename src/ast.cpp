#include "ast.hpp"
#include "asm.hpp"
#include "common.hpp"
#include "expr.h"
#include "ir.hpp"
#include "lang.hpp"
#include "types.hpp"
#include <fmt/base.h>
#include <fmt/ranges.h>
#include <magic_enum/magic_enum_flags.hpp>

namespace cmm::ast {

using namespace decl;

translation_unit* node::get_root() { return m_root; }
const translation_unit* node::get_root() const { return m_root; }

namespace {
template <typename... Ts>
std::vector<node*> lift_to_nodes(Ts*... ptrs) {
  return {static_cast<node*>(ptrs)...};
}
template <typename... Args>
std::vector<node*> concat_nodes(Args&&... args) {
  return lift_to_nodes(std::forward<Args>(args)...);
}
} // namespace

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

// std::string rank::string() const { return std::format("[{}]", number->string()); }

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
    : variable(specifiers(t), nullptr, std::move(id), init) {}

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

cmm::parameters decl::function::parameters_impl() const {
  return params | std::views::transform([](const variable& t_var) -> cmm::parameter {
           return {t_var.specs.type, t_var.init};
         }) |
         std::ranges::to<memory::vector<cmm::parameter>>();
}
std::vector<node*> decl::function::children() {
  return {to_node(&ident), to_node(&specs), to_node(&params), to_node(body)};
}

// std::string decl::function::repr() const { return std::format("{} {}", specs, ident); }
std::string decl::function::string() const { return std::format("{}", ident); }

assembly::element* decl::conversion_function::operator()(assembly::element*) const noexcept {
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

std::vector<function_store::value_type> function_store::get_by_name(const std::string& name) const {
  return data() | std::views::filter([name](const auto& pair) -> bool {
           std::string key = pair.first;
           if (size_t pos = key.find_first_of('_')) {
             return name == key.substr(0, pos);
           }
           return key == name;
         }) |
         std::views::values | std::ranges::to<std::vector>();
}

void function_store::insert(function* v) { hashmap::insert(v->ident.string(), v); }

[[nodiscard]] bool scope::is_declared(const ast::identifier& id) const noexcept {
  return variables.contains(id);
}

[[nodiscard]] const variable_store::value_type& scope::get_variable(
    const ast::identifier& t_ident) const {
  return variables.at(t_ident);
}
[[nodiscard]] const ast::decl::variable* scope::get_variable_declaration(
    const ast::identifier& id) const {
  return get_variable(id).first;
}
[[nodiscard]] variable_store::address_type scope::get_variable_address(
    const ast::identifier& t_id) const {
  return get_variable(t_id).second;
}

variable_store::address_type scope::declare_variable(ast::decl::variable* t_var) {
  auto* frame = find_parent<ast::function::definition>(t_var);
  frame->local_stack.push();
  auto offset = static_cast<assembly::offset_t>(frame->local_stack.size());
  auto* addr  = get_root()->cunit->get_operand<assembly::stack_memory>(offset);
  frame->variables.insert(t_var, addr);
  addr->hold_symbol(t_var);
  return addr;
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
    if (s->variables.contains(ident)) {
      return s->variables.at(ident);
    }
  }

  THROW(UNDECLARED_SYMBOL, ident);
}

assembly::reg* function::definition::declare_parameter(const variable& var, assembly::reg* op) {
  variables.insert(&var, op);
  op->hold_symbol(&var);
  return op;
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

constexpr auto create_overloaded_op(const std::vector<type_id>& types,
                                    const std::vector<instruction>& ins)
    -> std::vector<std::tuple<type_id, parameters, std::vector<instruction>>> {
  return types | std::views::transform([ins](type_id t) {
           return std::make_tuple(t, parameters{t, t}, ins);
         }) |
         TO_VEC;
}

template <instruction_t Ins>
constexpr auto create_overloaded_op(type_id ret, const std::vector<type_id>& types)
    -> std::vector<std::tuple<type_id, parameters, std::vector<instruction>>> {

  return types | std::views::transform([ret](type_id t) {
           return std::make_tuple(ret, parameters{t, t}, create_ins<Ins>());
         }) |
         TO_VEC;
}

constexpr auto provide_operator_overloads(
    operator_t op,
    const std::vector<std::tuple<type_id, parameters, std::vector<instruction>>>& ovs)
    -> std::pair<operator_t,
                 std::vector<std::tuple<type_id, parameters, std::vector<instruction>>>> {
  return std::make_pair(op, ovs);
}

using builtin_data = std::vector<
    std::pair<operator_t, std::vector<std::tuple<type_id, parameters, std::vector<instruction>>>>>;

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
           std::vector<std::tuple<type_id, parameters, std::vector<instruction>>> overloads =
               pair.second;
           auto callables =
               overloads |
               std::views::transform(
                   [op](const std::tuple<type_id, parameters, std::vector<instruction>>& data)
                       -> builtin_callable {
                     const auto& [ret, types, ins] = data;
                     return {op, ret, types, ins};
                   }) |
               TO_VEC;
           return std::make_pair(op, callables);
         }) |
         std::ranges::to<std::unordered_map>();
}
} // namespace
translation_unit::translation_unit(global_declarations decls)
    : stmts(std::move(decls)),
      cunit(nullptr) {}

void translation_unit::clear() noexcept {
  m_stackframe.clear();
  functions.clear();
  variables.clear();
}

const decl::label* translation_unit::get_label(const ast::identifier& id) const {
  return active_frame()->labels.at(id);
}

void translation_unit::create_frame(decl::function::definition* fn) { m_stackframe.push(fn); }

const variable_store::value_type& translation_unit::get_variable(const ast::identifier& id) const {
  if (variables.contains(id)) {
    return variables.at(id);
  }

  return active_frame()->get_variable(id);
}

variable_store::address_type translation_unit::declare_variable(ast::decl::variable* t_var) {
  assert(m_stackframe.empty());
  auto* addr = cunit->get_operand<assembly::label_memory>(t_var->ident);
  variables.insert(t_var, addr);
  addr->hold_symbol(t_var);
  cunit->reserve_static_var(t_var->ident.string());
  return addr;
}

void translation_unit::declare_function(decl::function* t_func) {
  REGISTER_TRACE("Creating func {}", t_func->ident.value());
  functions.insert(t_func);
  if (t_func->body != nullptr) {
    define_function(t_func);
  }
}

void translation_unit::define_function(ast::decl::function* t_func) {
  assert(t_func->body != nullptr);

  if (!functions.contains(t_func->ident)) {
    THROW(UNDECLARED_SYMBOL, t_func->ident);
  }

  const auto* declared_func = functions.at(t_func->ident);
  if (declared_func->body == nullptr) {
    THROW(ALREADY_DEFINED_FUNCTION, t_func->ident);
  }

  create_frame(t_func->body);
  auto registers = cunit->regs.parameters();
  for (const variable& param : t_func->params) {
    active_frame()->declare_parameter(param, registers.next());
  }
  active_frame()->local_stack.push();
  cunit->asmgen.load_new_procedure(t_func->ident.value());
  cunit->runner.generate_statements(*t_func->body);

  cunit->asmgen.save_current_procedure();

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

bool translation_unit::is_declarable(const ast::identifier& id) const noexcept {
  // Only check current scope
  return !active_scope()->variables.contains(id) && !functions.contains(id) &&
         !(!is_global_scope() ? !active_frame()->labels.contains(id) : true);
}

bool translation_unit::is_declared(const ast::identifier& id) const noexcept {
  // Only check current scope
  return variables.contains(id) || functions.contains(id);
}

std::vector<const ast::decl::function::definition*> translation_unit::stackframe() {
  std::vector<const ast::decl::function::definition*> res;
  while (!m_stackframe.empty()) {
    res.push_back(m_stackframe.top());
    m_stackframe.pop();
  }
  return res;
}

template <typename Id>
std::vector<const callable*> translation_unit::get_candidates(Id t_id) const {
  if constexpr (std::is_same_v<Id, ast::operator_>) {
    return translation_unit::operators.at(t_id.value()) |
           std ::views ::transform(
               [](const auto& elem) { return dynamic_cast<const callable*>(&elem); }) |
           std ::ranges ::to<std ::vector>();

  } else if constexpr (std::is_same_v<Id, ast::identifier>) {
    return CAST_RANGE(functions.get_by_name(t_id), const callable);
  }
}

template <typename Id>
const callable* translation_unit::get_callable(Id id,
                                               const expr::arguments& argument_expressions) const {
  auto argument_types = argument_expressions | MAP_RANGE(expr::expression*, elem->type());

  std::vector<const callable*> overloads = get_candidates<Id>(id);
  auto possible_exact_match =
      overloads | std::views::filter([argument_types](const callable* fn) -> bool {
        return std::ranges::equal(
            (fn->parameters() |
             std ::views ::transform([](const parameter& param) -> type_id { return param.type; })),
            argument_types);
      }) |
      TO_VEC;

  if (possible_exact_match.size() == 1) {
    return dynamic_cast<const callable*>(possible_exact_match.front());
  }

  // Overload resolution
  const auto* resolved = resolve_overloads(overloads, argument_expressions);
  if (!resolved) {
    THROW(UNDECLARED_SYMBOL, id);
  }

  return dynamic_cast<const callable*>(resolved);
}

const callable* translation_unit::resolve_overloads(std::vector<const callable*> candidates,
                                                    const expr::arguments& argument_expressions) {
  // Already have candidates
  // Lets check number of parameters
  auto check_param_number = [argument_expressions](const callable* fn) {
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
  REGISTER_TRACE("Tras step1 quedan {}", step1.size());
  auto check_binding_rules = [argument_expressions](const callable* fn) {
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
                expression->value_category() == value_category_t::RVALUE;
            // Or lvalue arg with rvalue reference parameter
            bool lvaluearg_rvaluerefparam =
                types::is_rvalue(parameter.type).ok &&
                expression->value_category() == value_category_t::LVALUE;
            // Is not viable function
            return !rvaluearg_nonconstlvalue && !lvaluearg_rvaluerefparam;
          }

          return std::ranges::any_of(standard_conversions, [&expression](const conversor* conv) {
            return conv->is_convertible(expression->value_category(), expression->type());
          });
        });
  };
  auto step2 = step1 | std::views::filter(check_binding_rules) | TO_VEC;
  REGISTER_TRACE("Tras step2 quedan {}", step2.size());
  if (step2.size() == 0) {
    return nullptr;
  }
  return step2.front();
}

[[nodiscard]] bool translation_unit::match_arguments(const std::vector<type_id>& builtin,
                                                     const std::vector<type_id>& called) {
  return std::ranges::all_of(std::views::zip(builtin, called), [](const auto& pair) {
    const auto& [required, actual] = pair;
    return required == actual;
  });
}
cmm::binding_mode_t translation_unit::bind_value(value_category_t cat, type_id type) {
  auto mode = binding_rules.at(cat) |
              FILTER([type](const auto& rule) { return rule.first(type).ok; }) |
              std::views::values | TO_VEC;
  if (mode.empty()) {
    THROW(NOT_BINDEABLE, type, cat);
  }
  if (mode.size() > 1) {
    REGISTER_WARN("Too many results when binding values, {}", fmt::join(mode, ", "));
  }
  static_assert(std::formattable<binding_mode_t, char>);
  static_assert(fmt::is_formattable<binding_mode_t>::value);
  return mode.front();
}

ast::expr::expression* translation_unit::bind_expression(ast::expr::expression* t_expr,
                                                         type_id t_param) {
  auto mode = translation_unit::bind_value(t_expr->value_category(), t_param);
  using enum value_category_t;
  using enum binding_mode_t;
  switch (magic_enum::enum_fuse(t_expr->value_category(), mode).value()) {
    case magic_enum::enum_fuse(LVALUE, COPY).value():
      {
        t_expr->current_conversor = lvalue_to_rvalue;
        return t_expr;
      }
      break;
    default:
      return t_expr;
  }
}
bool translation_unit::is_bindable_to(value_category_t t_value, types::type_id t_type) {
  return std::ranges::any_of(binding_rules[t_value],
                             [t_type](const auto& t_rule) { return t_rule.first(t_type); });
}

template const callable* translation_unit::get_callable(
    ast::identifier id,
    const expr::arguments& argument_expressions) const;
template const callable* translation_unit::get_callable(
    ast::operator_ id,
    const expr::arguments& argument_expressions) const;

decltype(translation_unit::operators) translation_unit::operators = provide_builtin_callables();
decltype(translation_unit::unary_matchers) translation_unit::unary_matchers =
    &types::unary_matchers;
decltype(translation_unit::modifiers) translation_unit::modifiers = &types::modifiers;
decltype(translation_unit::types) translation_unit::types         = &types::manager::instance();
decltype(translation_unit::standard_conversions) translation_unit::standard_conversions = {
    &lvalue_to_rvalue,
    &any_to_bool};
} // namespace cmm::ast
