#include "lang.hpp"

#include "asm.hpp"
#include <utility>

#include "ast.hpp"
#include "common.hpp"
#include "types.hpp"

namespace cmm {

[[nodiscard]] ast::decl::signature builtin_signature_data::signature() const {
  return {ast::identifier(std::string(function_name)), args};
}

mangled_name mangled_name::function(cstring name, const std::vector<const type*>& t) {
  return std::format("{}_{}", name, types(t));
}

std::string mangled_name::types(const std::vector<ptype>& types) {
  return types | std::views::transform([](ptype t) { return t->string(); }) |
         std::views::join_with('_') | std::ranges::to<std::string>();
}

mangled_name mangled_name::direct_conversion_function(crtype f, crtype t) {
  return function("conv_{}", std::vector<ptype>{&f, &t});
}

mangled_name mangled_name::label(cstring name) { return std::string(name); }
mangled_name mangled_name::variable(cstring name, crtype) { return std::string(name); }

signature::signature(operator_t op, std::vector<ptype_spec> types)
    : name(std::format("operator{}", op)),
      argument_types(std::move(types)) {}
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
} // namespace
const std::unordered_map<operator_t, operator_builtin_data> builtin_operators{
    {operator_t::assign,
     {&matchers::is_ref, &matchers::is_ref, &matchers::is_cref, create_ins<instruction_t::mov>()}    },

    // Unary
    {operator_t::pre_inc,  {SINTREF_T, SINTREF_T, VOID_T, create_ins<instruction_t::inc>()}          },
    {operator_t::pre_dec,  {SINTREF_T, SINTREF_T, VOID_T, create_ins<instruction_t::dec>()}          },
    {operator_t::post_inc, {SINT_T, SINTREF_T, SINT_T, create_ins<instruction_t::inc>()}             },
    {operator_t::post_dec, {SINT_T, SINTREF_T, SINT_T, create_ins<instruction_t::dec>()}             },
    // Comparisons
    {operator_t::eq,       {BOOL_T, &matchers::any, &matchers::any, create_ins<instruction_t::mov>()}},
    {operator_t::neq,      {BOOL_T, &matchers::any, &matchers::any, create_ins<instruction_t::mov>()}},
    {operator_t::gt,
     {BOOL_T, &matchers::is_integral, &matchers::is_integral, create_ins<instruction_t::mov>()}      },
    {operator_t::ge,
     {BOOL_T, &matchers::is_integral, &matchers::is_integral, create_ins<instruction_t::mov>()}      },
    {operator_t::lt,
     {BOOL_T, &matchers::is_integral, &matchers::is_integral, create_ins<instruction_t::mov>()}      },
    {operator_t::le,
     {BOOL_T, &matchers::is_integral, &matchers::is_integral, create_ins<instruction_t::mov>()}      },
    // Arithmetic
    {operator_t::plus,
     {&matchers::is_integral,
      &matchers::is_integral,
      &matchers::is_integral,
      create_ins<instruction_t::add>()}                                                              },
    {operator_t::minus,
     {&matchers::is_integral,
      &matchers::is_integral,
      &matchers::is_integral,
      create_ins<instruction_t::sub>()}                                                              },
    {operator_t::star,
     {&matchers::is_integral,
      &matchers::is_integral,
      &matchers::is_integral,
      {single_ins<instruction_t::mov>(arg_t::ACC, arg_t::LEFT),
       single_ins<instruction_t::mul>(arg_t::RIGHT),
       single_ins<instruction_t::mov>(arg_t::LEFT, arg_t::ACC)}}                                     },
    {operator_t::fslash,
     {&matchers::is_integral,
      &matchers::is_integral,
      &matchers::is_integral,
      {single_ins<instruction_t::mov>(arg_t::ACC, arg_t::LEFT),
       single_ins<instruction_t::div>(arg_t::RIGHT),
       single_ins<instruction_t::mov>(arg_t::LEFT, arg_t::ACC)}}                                     },
    {operator_t::xor_,
     {&matchers::any, &matchers::any, &matchers::any, create_ins<instruction_t::xor_>()}             },
    {operator_t::or_,
     {&matchers::any, &matchers::any, &matchers::any, create_ins<instruction_t::or_>()}              },
    {operator_t::and_,
     {&matchers::any, &matchers::any, &matchers::any, create_ins<instruction_t::and_>()}             },
    {operator_t::not_,
     {&matchers::any, &matchers::any, VOID_T, create_ins<instruction_t::not_>()}                     }
};

namespace {
constexpr bool try_match(ptype t, ptype p) {
  if (const auto* matcher = dynamic_cast<const type_matcher*>(p)) {
    return matcher->match(*t);
  }
  return t == p;
}
} // namespace
std::optional<operator_builtin_data> get_builtin_operator(operator_t op,
                                                          const std::vector<ptype>& types) {
  const auto& data = builtin_operators.at(op);
  if (try_match(types.at(1), data.arg2) && try_match(types.at(0), data.arg1)) {
    return data;
  }
  return {};
}
} // namespace cmm
