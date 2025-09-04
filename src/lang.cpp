#include "lang.hpp"

#include "asm.hpp"
#include <utility>

#include "common.hpp"
#include "types.hpp"

using cmm::arg_t;
using cmm::builtin_operator_data;
using cmm::conversor;
using cmm::instruction;
using cmm::instruction_t;
using cmm::operator_t;
using cmm::value_category_t;
using cmm::types::type_id;

[[nodiscard]] cmm::callable_contract cmm::builtin_signature_data::contract() const {
  return {VOID_T, std::string(function_name), args};
}

cmm::mangled_name cmm::mangled_name::function(std::string_view name,
                                              const std::vector<types::type_id>& t) {
  if (t.size() == 0) {
    return std::format("{}", name);
  }
  return std::format("{}_{}", name, types(t));
}

std::string cmm::mangled_name::types(const std::vector<types::type_id>& type_ids) {
  return type_ids | std::views::transform([](const auto& t) { return t->string(); }) |
         std::views::join_with('_') | std::ranges::to<std::string>();
}

cmm::mangled_name cmm::mangled_name::label(std::string_view name) { return std::string(name); }

cmm::mangled_name cmm::mangled_name::variable(std::string_view name, types::type_id) {
  return std::string(name);
}

cmm::callable_signature::callable_signature(std::string_view op,
                                            std::vector<types::type_id> type_ids)
    : name(op),
      argument_types(std::move(type_ids)) {}

cmm::mangled_name::operator std::string() const { return m_string; }

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
  for (const type_id& unary_matcher : args) {
    res.push_back(create_same_args<Ins>(unary_matcher));
  }
  return res;
}

template <instruction_t Ins>
constexpr std::vector<builtin_operator_data> create_overloaded_op(
    const type_id& ret,
    const std::vector<type_id>& args) {
  std::vector<builtin_operator_data> res;
  res.reserve(args.size());
  for (const type_id& unary_matcher : args) {
    res.push_back(create_same_args<Ins>(ret, unary_matcher));
  }
  return res;
}

constexpr std::vector<builtin_operator_data> create_overloaded_op(const std::vector<type_id>& args,
                                                                  std::vector<instruction> ins) {
  std::vector<builtin_operator_data> res;
  res.reserve(args.size());
  for (const type_id& unary_matcher : args) {
    res.emplace_back(unary_matcher, unary_matcher, unary_matcher, std::move(ins));
  }
  return res;
}
} // namespace

bool conversor::is_convertible(type_id t) const { return m_from(t).ok; }

type_id conversor::operator()(type_id t) const {
  if (!is_convertible(t)) {
    throw error(std::format("Type {} is not convertible with {}", t, *this));
  }
  return m_modifier(t);
}

std::vector<type_id> cmm::get_convertible_types(type_id from) {
  return cmm::standard_conversions |
         std::views::filter([from](const conversor* conv) { return conv->is_convertible(from); }) |
         std::views::transform([&from](const conversor* converter) { return (*converter)(from); }) |
         std::ranges::to<std::vector>();
}

bool cmm::is_convertible(type_id from, type_id to) {
  if (from == to) {
    return true;
  }
  auto converted =
      get_convertible_types(from) | std::views::filter([to](type_id t) { return t == to; });
  return !std::ranges::empty(converted);
}

const std::unordered_map<cmm::operator_t, std::vector<builtin_operator_data>>
    cmm::builtin_operators{
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
         create_overloaded_op<instruction_t::mov>(BOOL_T,
         {BOOL_T, SINT_T, UINT_T, FLOAT_T, CHAR_T})},
        {operator_t::neq,
         create_overloaded_op<instruction_t::mov>(BOOL_T,
         {BOOL_T, SINT_T, UINT_T, FLOAT_T, CHAR_T})},
        {operator_t::gt,
         create_overloaded_op<instruction_t::mov>(BOOL_T,
         {BOOL_T, SINT_T, UINT_T, FLOAT_T, CHAR_T})},
        {operator_t::ge,
         create_overloaded_op<instruction_t::mov>(BOOL_T,
         {BOOL_T, SINT_T, UINT_T, FLOAT_T, CHAR_T})},
        {operator_t::lt,
         create_overloaded_op<instruction_t::mov>(BOOL_T,
         {BOOL_T, SINT_T, UINT_T, FLOAT_T, CHAR_T})},
        {operator_t::le,
         create_overloaded_op<instruction_t::mov>(BOOL_T,
         {BOOL_T, SINT_T, UINT_T, FLOAT_T, CHAR_T})},
        // Arithmetic
        {operator_t::plus,
         create_overloaded_op<instruction_t::add>({BOOL_T, SINT_T, UINT_T, FLOAT_T})},
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
        // {operator_t::or_, {{unary_matchers::any, {}, {}, create_ins<instruction_t::or_>()}}},
        // {operator_t::xor_, {{unary_matchers::any, {}, {}, create_ins<instruction_t::xor_>()}}},
        // {operator_t::and_, {{unary_matchers::any, {}, {}, create_ins<instruction_t::and_>()}}},
        // {operator_t::not_,     {unary_matchers::any, {}, VOID_T,
        // create_ins<instruction_t::not_>()}       }
};

std::vector<const builtin_operator_data*> cmm::get_builtin_operator(operator_t op) {
  auto pointer_of = [](const builtin_operator_data& v) -> const builtin_operator_data* {
    return &v;
  };
  return cmm::builtin_operators.at(op) | TRANSFORM(pointer_of) | TO_VEC;
}

value_category_t cmm::get_value_category(type_id t) {
  if (cmm::types::is_lvalue(t).ok) {
    return value_category_t::LVALUE;
  }
  if (cmm::types::is_rvalue(t).ok) {
    return value_category_t::XVALUE;
  }
  return value_category_t::PRVALUE;
}

bool cmm::is_bindable_to(value_category_t value_category, type_id type) {
  return std::ranges::any_of(binding_rules.at(value_category),
                             [type](const auto& rule) { return rule.first(type).ok; });
}

cmm::binding_mode_t cmm::bind_value(value_category_t cat, type_id type) {
  auto mode = binding_rules.at(cat) |
              FILTER([type](const auto& rule) { return rule.first(type).ok; }) |
              std::views::values | TO_VEC;
  if (mode.empty()) {
    THROW(NOT_BINDEABLE, type, cat);
  }
  if (mode.size() > 1) {
    REGISTER_WARN("Too many results when binding values, {}", mode);
  }
  return mode.front();
}
