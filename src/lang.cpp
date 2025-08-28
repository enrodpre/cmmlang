#include "lang.hpp"

#include "asm.hpp"
#include <utility>

#include "common.hpp"
#include "types.hpp"

namespace cmm {

[[nodiscard]] callable_contract builtin_signature_data::contract() const {
  return {VOID_T, std::string(function_name), args};
}

mangled_name mangled_name::function(cstring name, const std::vector<type_id>& t) {
  if (t.size() == 0) {
    return std::format("{}", name);
  }
  return std::format("{}_{}", name, types(t));
}

std::string mangled_name::types(const std::vector<type_id>& type_ids) {
  return type_ids | std::views::transform([](const auto& t) { return t->string(); }) |
         std::views::join_with('_') | std::ranges::to<std::string>();
}

mangled_name mangled_name::label(cstring name) { return std::string(name); }

mangled_name mangled_name::variable(cstring name, type_id) { return std::string(name); }

callable_signature::callable_signature(cstring op, std::vector<type_id> type_ids)
    : name(op),
      argument_types(std::move(type_ids)) {}

mangled_name::operator std::string() const { return m_string; }

namespace {
template <instruction_t Ins>
instruction single_ins(arg_t l = arg_t::LEFT, arg_t r = arg_t::RIGHT) {
  return instruction{.ins = Ins, .arg1 = l, .arg2 = r};
} // namespace

template <instruction_t Ins>
std::vector<instruction> create_ins(arg_t l = arg_t::LEFT, arg_t r = arg_t::RIGHT) {
  return {
      instruction{.ins = Ins, .arg1 = l, .arg2 = r}
  };
}

template <instruction_t Ins>
constexpr builtin_operator_data create_same_args(const type_id& args) {
  return {args, args, args, create_ins<Ins>()};
}

template <instruction_t Ins>
constexpr builtin_operator_data create_same_args(type_id ret, const type_id& args) {
  return {std::move(ret), args, args, create_ins<Ins>()};
}

template <instruction_t Ins>
constexpr std::vector<builtin_operator_data> create_overloaded_op(
    const std::vector<type_id>& args) {
  std::vector<builtin_operator_data> res;
  res.reserve(args.size());
  for (const type_id& matcher : args) {
    res.push_back(create_same_args<Ins>(matcher));
  }
  return res;
}

template <instruction_t Ins>
constexpr std::vector<builtin_operator_data> create_overloaded_op(
    const type_id& ret,
    const std::vector<type_id>& args) {
  std::vector<builtin_operator_data> res;
  res.reserve(args.size());
  for (const type_id& matcher : args) {
    res.push_back(create_same_args<Ins>(ret, matcher));
  }
  return res;
}

constexpr std::vector<builtin_operator_data> create_overloaded_op(const std::vector<type_id>& args,
                                                                  std::vector<instruction> ins) {
  std::vector<builtin_operator_data> res;
  res.reserve(args.size());
  for (const type_id& matcher : args) {
    res.emplace_back(matcher, matcher, matcher, std::move(ins));
  }
  return res;
}
} // namespace

const std::unordered_map<operator_t, std::vector<builtin_operator_data>> builtin_operators{
    {operator_t::assign,
     {{SINTREF_T, SINTREF_T, SINT_T, create_ins<instruction_t::mov>()},
      {UINTREF_T, UINTREF_T, UINT_T, create_ins<instruction_t::mov>()},
      {FLOATREF_T, FLOATREF_T, FLOAT_T, create_ins<instruction_t::mov>()}}},

    // Unary
    {operator_t::pre_inc, {{SINTREF_T, SINTREF_T, create_ins<instruction_t::inc>()}}},
    {operator_t::pre_dec, {{SINTREF_T, SINTREF_T, create_ins<instruction_t::dec>()}}},
    {operator_t::post_inc, {{SINT_T, SINTREF_T, SINT_T, create_ins<instruction_t::inc>()}}},
    {operator_t::post_dec, {{SINT_T, SINTREF_T, SINT_T, create_ins<instruction_t::dec>()}}},
    // Comparisons
    {operator_t::eq,
     create_overloaded_op<instruction_t::mov>(BOOL_T, {BOOL_T, SINT_T, UINT_T, FLOAT_T, CHAR_T})},
    {operator_t::neq,
     create_overloaded_op<instruction_t::mov>(BOOL_T, {BOOL_T, SINT_T, UINT_T, FLOAT_T, CHAR_T})},
    {operator_t::gt,
     create_overloaded_op<instruction_t::mov>(BOOL_T, {BOOL_T, SINT_T, UINT_T, FLOAT_T, CHAR_T})},
    {operator_t::ge,
     create_overloaded_op<instruction_t::mov>(BOOL_T, {BOOL_T, SINT_T, UINT_T, FLOAT_T, CHAR_T})},
    {operator_t::lt,
     create_overloaded_op<instruction_t::mov>(BOOL_T, {BOOL_T, SINT_T, UINT_T, FLOAT_T, CHAR_T})},
    {operator_t::le,
     create_overloaded_op<instruction_t::mov>(BOOL_T, {BOOL_T, SINT_T, UINT_T, FLOAT_T, CHAR_T})},
    // Arithmetic
    {operator_t::plus, create_overloaded_op<instruction_t::add>({BOOL_T, SINT_T, UINT_T, FLOAT_T})},
    {operator_t::minus, create_overloaded_op<instruction_t::sub>({SINT_T, UINT_T, FLOAT_T})},
    {operator_t::star,
     create_overloaded_op({SINT_T, UINT_T, FLOAT_T},
     {single_ins<instruction_t::mov>(arg_t::ACC, arg_t::LEFT),
                           single_ins<instruction_t::mul>(arg_t::RIGHT),
                           single_ins<instruction_t::mov>(arg_t::LEFT, arg_t::ACC)})},
    {operator_t::fslash,
     create_overloaded_op({SINT_T, UINT_T, FLOAT_T},
     {single_ins<instruction_t::mov>(arg_t::ACC, arg_t::LEFT),
                           single_ins<instruction_t::mul>(arg_t::RIGHT),
                           single_ins<instruction_t::mov>(arg_t::LEFT, arg_t::ACC)})},
    // {operator_t::or_, {{matchers::any, {}, {}, create_ins<instruction_t::or_>()}}},
    // {operator_t::xor_, {{matchers::any, {}, {}, create_ins<instruction_t::xor_>()}}},
    // {operator_t::and_, {{matchers::any, {}, {}, create_ins<instruction_t::and_>()}}},
    // {operator_t::not_,     {matchers::any, {}, VOID_T,
    // create_ins<instruction_t::not_>()}       }
};

std::vector<const builtin_operator_data*> get_builtin_operator(operator_t op) {
  auto pointer_of = [](const builtin_operator_data& v) -> const builtin_operator_data* {
    return &v;
  };
  return builtin_operators.at(op) | TRANSFORM(pointer_of) | TO_VEC;
}

value_category_t get_value_category(type_id t) {
  if (matchers::is_lvalue(t)) {
    return value_category_t::LVALUE;
  }
  if (matchers::is_rvalue(t)) {
    return value_category_t::XVALUE;
  }
  return value_category_t::PRVALUE;
}
std::vector<type_id> get_bindable_candidates(value_category_t cat, type_id arg) {
  return binding_rules.at(cat) |
         FILTER([arg](const binding_rule& rule) { return rule.matcher(arg); }) |
         TRANSFORM([arg](const binding_rule& rule) { return rule.modifier(arg); }) | TO_VEC;
}

bool is_bindeable(value_category_t value_category, type_id arg, type_id param) {
  return std::ranges::contains(get_bindable_candidates(value_category, arg), param);
}

type_id bind_argument(value_category_t cat, type_id arg, type_id param) {
  if (!is_bindeable(cat, arg, param)) {
    THROW(BAD_FUNCTION_CALL, arg);
  }
  return param;
}
} // namespace cmm
