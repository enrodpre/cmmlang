#pragma once

#include "asm.hpp"
#include "common.hpp"
#include "ir.hpp"
#include "lang.hpp"
#include "types.hpp"
#include <libassert/assert.hpp>
#include <ranges>
#include <tuple>
#include <utility>

namespace cmm::ir {

static_assert(std::formattable<const instruction_t&, char>);
template <typename Decl>
symbol<Decl>::symbol(const Decl* decl, mangled_name&& mname, address_t addr)
    : decl(decl),
      name(std::move(mname)),
      addr(addr) {}

template <typename T, typename... Args>
  requires std::is_constructible_v<T, Args...>
const variable_store::value_type& variable_store::emplace(key_type k, Args&&... args) {
  const auto& [it, ok] = m_store.emplace(std::make_pair(k, T(std::forward<Args>(args)...)));
  return it->second;
}

template <typename T>
bool symbol_table::is_declarable(symbol_table::identifier_type ident) const noexcept {
  if constexpr (std::is_same_v<ir::variable, T>) {
    // Only check current scope
    return !active_scope().variables.contains(ident.value);
  } else if constexpr (std::is_same_v<ir::function, T>) {
    return m_functions.get_by_name(ident.value).size() > 0;
  } else {
    return active_frame().labels.contains(ident.value);
  }
}

template <typename T>
bool symbol_table::is_declared(symbol_table::identifier_type ident) const noexcept {
  if constexpr (std::is_same_v<ir::variable, T>) {
    // Only check current scope

    return (!is_global_scope() && !m_stackframe.empty() && active_frame().is_declared(ident)) ||
           m_global_scope.variables.contains(ident.value);
  } else if constexpr (std::is_same_v<ir::function, T>) {
    return m_functions.get_by_name(ident.value).size() > 0;
  } else {
    return active_frame().labels.contains(ident.value);
  }
}

template <typename... Args>
void compilation_unit::instruction(const instruction_t& ins, Args&&... args) {

  if constexpr ((sizeof...(Args) == 0)) {
    asmgen.write_instruction(ins);
  } else if constexpr ((sizeof...(Args) == 1)) {

    auto op = std::get<0>(std::forward_as_tuple(args...));
    if (!instruction_t(ins).can_address_memory) {
      // Use an aux register to load the value and then put it back onto the
      // address
      REGISTER_WARN("{} cannot address memory", ins.format());
      static_assert(std::formattable<cmm::instruction_t, char>);
      if (!op->empty() &&
          op->content()->attribute == operand::symbol_container::symbol_attr::ADDRESS) {
        auto* aux = regs.get(assembly::registers::AUX);
        asmgen.write_instruction(instruction_t::mov, aux->value(), op->value());
        asmgen.write_instruction(ins, aux->value());
        asmgen.write_instruction(instruction_t::mov, op->value(), aux->value());
        return;
      }
    }
    asmgen.write_instruction(ins, op->value());
  } else if constexpr ((sizeof...(Args) == 2)) {
    asmgen.write_instruction(ins, std::forward<Args>(args)...);
  }
}

namespace builtin {
  namespace function {
    [[nodiscard]] constexpr const builtin_signature_t::properties_map&
    builtin_signature_t::properties_array() {
      static properties_map MAP{{{{"exit", {UINT_T}}}}};
      return MAP;
    }
    namespace preprocessors {
      using preprocess_t                = ir::builtin_function::preprocess_t;
      const preprocess_t SIMPLE_LOADING = [](compilation_unit& v,
                                             const builtin_function::parameters_t& params,
                                             const std::vector<ast::expr::expression*>& args) {
        return args | std::views::enumerate | std::views::transform([&](const auto& pair) {
                 const auto& [i, arg] = pair;
                 return v.runner.generate_expr(*arg,
                                               cmm::types::is_indirect_v::operator()(*params.at(i))
                                                   ? intents::intent_t::LOAD_VARIABLE_ADDRESS
                                                   : intents::intent_t::LOAD_VARIABLE_VALUE,
                                               v.regs.parameters.at(i));
               }) |
               std::ranges::to<std::vector>();
      };
    } // namespace preprocessors
    namespace bodies {
      using body_t      = ir::builtin_function::body_t;

      const body_t EXIT = [](compilation_unit& v, const std::vector<operand*>& arg) -> operand* {
        v.current_phase = Phase::EXITING;
        v.move(v.regs.parameters.at(0), arg[0]);
        v.jump("exit");
        return nullptr;
      };

      const body_t IDENTITY = [](compilation_unit&, const std::vector<operand*>& args) -> operand* {
        return args[0];
      };
      const body_t CAST_TO_BOOL = [](compilation_unit& v,
                                     const std::vector<operand*>& args) -> operand* {
        auto* arg = args[0];
        auto* aux = v.regs.parameters.next();
        v.move_immediate(aux, std::to_string(assembly::bits::masks::TO_BOOL));
        v.instruction(instruction_t::and_, arg, aux);
        return arg;
      };
    } // namespace bodies
    namespace postprocessors {
      using post_t               = builtin_function::postprocess_t;
      const post_t SIMPLE_RET    = [](compilation_unit&, operand* res) -> operand* { return res; };
      const post_t SAVE_VARIABLE = [](compilation_unit& v, operand* res) -> operand* {
        if (const auto* var = res->variable()) {
          v.save_variable(var, res);
          return res;
        }
        UNREACHABLE("aaaa");
      };

      static_assert(std::is_assignable_v<builtin_function::postprocess_t, decltype(SIMPLE_RET)>);
    }; // namespace postprocessors
  }; // namespace function
  namespace conversions {
    namespace conditions {
      template <type_category_t T>
      constexpr auto BELONGS_TO = [](cr_type t) -> bool { return types::belongs_to(t, T); };
    }; // namespace conditions
    namespace extractors {
      constexpr auto EXTRACT_TYPE = [](cr_type t) -> cr_type { return *t.underlying; };
      constexpr auto WRAP_REF     = [](cr_type t) -> cr_type {
        return type::create_lvalue(&t, t.c, t.v);
      };
      template <type_category_t T>
      constexpr auto GENERATE_TYPE = [](cr_type) -> cr_type { return type::create(T); };
    }; // namespace extractors
  } // namespace conversions
} // namespace builtin
constexpr void builtin::provider::create_direct_conversion(cr_type from,
                                                           cr_type to,
                                                           builtin_function::body_t b) {
  table.m_conversions.emplace_direct(std::move(b), from, to);
}
constexpr void builtin::provider::create_glob_conversion(std::string mangled,
                                                         glob_conversion_function::condition_t cond,
                                                         glob_conversion_function::extractor_t extr,
                                                         const builtin_function::body_t& b) {
  table.m_conversions.emplace_glob(std::move(mangled), b, std::move(cond), std::move(extr));
}
constexpr void builtin::provider::create_function(std::optional<ptr_type> ret,
                                                  std::string&& name,
                                                  const std::vector<const type*>& args,
                                                  const builtin_function::descriptor_t& desc) {
  table.m_functions.emplace_builtin(std::move(name), ret, args, desc, false);
}
constexpr void builtin::provider::create_builtin_function(
    std::optional<ptr_type> ret,
    const function::builtin_signature_t& sig,
    const builtin_function::descriptor_t& desc) {
  create_function(ret, std::string(sig.function_name), sig.args, desc);
}
constexpr void builtin::provider::create_builtin_operator(
    ptr_type ret,
    const operator_t& op,
    const std::vector<ptr_type>& args,
    const builtin_function::descriptor_t& desc) {
  create_function(ret, op.caller_function(), args, desc);
}
template <_instruction_t Ins>
constexpr void builtin::provider::create_simple_operator(const operator_t& op, ptr_type type) {
  using namespace function;
  constexpr auto N = instruction_t(Ins).n_params;
  if constexpr (N == 1) {
    create_builtin_operator(type, op, {type}, {bodies::EXECUTE_INSTRUC<Ins>});
  } else if constexpr (N == 2) {
    create_builtin_operator(type, op, {type, type}, {bodies::EXECUTE_INSTRUC<Ins>});
  }
}

constexpr void builtin::provider::provide_operators() {
  using namespace function;
  create_builtin_operator(
      SINTREF_T, operator_t::assign, {SINTREF_T, SINT_T}, {bodies::EXECUTE_MOVE});

  create_builtin_operator(
      SINTREF_T, operator_t::pre_inc, {SINTREF_T}, {bodies::EXECUTE_INSTRUC<_instruction_t::inc>});
  create_builtin_operator(
      SINTREF_T, operator_t::pre_dec, {SINTREF_T}, {bodies::EXECUTE_INSTRUC<_instruction_t::dec>});
  create_builtin_operator(
      SINTREF_T, operator_t::post_dec, {SINTREF_T}, {bodies::POST_UNARY<_instruction_t::dec>});
  create_builtin_operator(
      SINTREF_T, operator_t::post_inc, {SINTREF_T}, {bodies::POST_UNARY<_instruction_t::inc>});

  create_simple_operator<instruction_t::add>(operator_t::plus, SINT_T);
  create_simple_operator<instruction_t::sub>(operator_t::minus, SINT_T);
  create_simple_operator<instruction_t::mul>(operator_t::star, SINT_T);
  create_simple_operator<instruction_t::div>(operator_t::fslash, SINT_T);
  create_simple_operator<instruction_t::cmp>(operator_t::eq, SINT_T);
  create_simple_operator<instruction_t::cmp>(operator_t::neq, SINT_T);
  create_simple_operator<instruction_t::cmp>(operator_t::le, SINT_T);
  create_simple_operator<instruction_t::cmp>(operator_t::lt, SINT_T);
  create_simple_operator<instruction_t::cmp>(operator_t::ge, SINT_T);
  create_simple_operator<instruction_t::cmp>(operator_t::gt, SINT_T);
  create_simple_operator<instruction_t::and_>(operator_t::and_, SINT_T);
  create_simple_operator<instruction_t::or_>(operator_t::or_, SINT_T);
  create_simple_operator<instruction_t::not_>(operator_t::not_, SINT_T);
  create_simple_operator<instruction_t::not_>(operator_t::star, SINT_T);
  create_simple_operator<instruction_t::not_>(operator_t::ampersand, SINT_T);
}

constexpr void builtin::provider::provide_functions() {
  using namespace function;
  create_builtin_function({}, builtin_signature_t::EXIT, {bodies::EXIT});
}

constexpr void builtin::provider::provide_conversions() {
  create_glob_conversion("conv_lvalue_type",
                         conversions::conditions::BELONGS_TO<type_category_t::lvalue_ref_t>,
                         conversions::extractors::EXTRACT_TYPE,
                         function::bodies::IDENTITY);
  create_glob_conversion("conv_type_lvalue",
                         conversions::conditions::BELONGS_TO<type_category_t::fundamental_t>,
                         conversions::extractors::WRAP_REF,
                         function::bodies::IDENTITY);
  create_glob_conversion("conv_fundamental_bool",
                         conversions::conditions::BELONGS_TO<type_category_t::fundamental_t>,
                         conversions::extractors::GENERATE_TYPE<type_category_t::bool_t>,
                         function::bodies::CAST_TO_BOOL);

  create_direct_conversion(*UINT_T, *SINT_T, function::bodies::IDENTITY);
}
constexpr void builtin::provider::provide() {
  provide_operators();
  provide_functions();
  provide_conversions();
}

} // namespace cmm::ir
