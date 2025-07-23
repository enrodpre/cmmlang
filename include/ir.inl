#pragma once

#include "asm.hpp"
#include "ir.hpp"
#include "lang.hpp"
#include <libassert/assert.hpp>
#include <tuple>
#include <utility>

namespace cmm::ir {

static_assert(std::formattable<const instruction_t&, char>);
template <typename Decl>
symbol<Decl>::symbol(const Decl* decl, address_t addr)
    : decl(decl),
      addr(addr) {}

template <typename T, typename... Args>
  requires std::is_constructible_v<T, Args...>
const variable_store::value_type& variable_store::emplace(key_type k,
                                                          Args&&... args) {
  const auto& [it, ok] =
      m_store.emplace(std::make_pair(k, T(std::forward<Args>(args)...)));
  return it->second;
}

template <typename T>
bool symbol_table::is_declarable(
    symbol_table::identifier_type ident) const noexcept {
  if constexpr (std::is_same_v<ir::variable, T>) {
    // Only check current scope
    bool global_conflict =
        is_global_scope() && m_global_scope.variables.contains(ident.value);

    return global_conflict || active_scope().variables.contains(ident.value);
  } else if constexpr (std::is_same_v<ir::function, T>) {
    return m_functions.get(ident.value).size() > 0;
  } else {
    return active_frame().labels.contains(ident.value);
  }
}

template <typename T>
bool symbol_table::is_declared(
    symbol_table::identifier_type ident) const noexcept {
  if constexpr (std::is_same_v<ir::variable, T>) {
    // Only check current scope

    return (!is_global_scope() && !m_stackframe.empty() &&
            active_frame().is_declared(ident)) ||
           m_global_scope.variables.contains(ident.value);
  } else if constexpr (std::is_same_v<ir::function, T>) {
    return m_functions.get(ident.value).size() > 0;
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
      if (!op->empty() && op->content()->attribute ==
                              operand::symbol_container::symbol_attr::ADDRESS) {
        auto* aux = regs.get(assembly::registers::AUX);
        asmgen.write_instruction(instruction_t::mov, aux->value(), op->value());
        asmgen.write_instruction(ins, aux->value());
        asmgen.write_instruction(instruction_t::mov, op->value(), aux->value());
        return;
      }
    }
    asmgen.write_instruction(ins, op->value());
  } else if constexpr ((sizeof...(Args) == 0)) {
    asmgen.write_instruction(ins, std::forward<Args>(args)...);
  }
}

namespace {
  constexpr assembly::operand* load_parameter(compilation_unit& v,
                                              cv_type type,
                                              ast::expr::expression* expr,
                                              assembly::reg* to) {

    return v.runner.generate_expr(*expr,
                                  type->is_reference()
                                      ? intents::intent_t::LOAD_VARIABLE_ADDRESS
                                      : intents::intent_t::LOAD_VARIABLE_VALUE,
                                  to);
  }
}; // namespace
namespace builtin {
  namespace function {
    [[nodiscard]] constexpr const header_t::properties_map&
    header_t::properties_array() {
      static constexpr properties_map MAP{
          {{{"exit", types::VOID_T, {types::UINT_T}}}}};
      return MAP;
    }
    constexpr std::string header_t::mangle() const {
      return mangled_name::builtin_function(name, {parameter_t}).str();
    }

    namespace preprocessors {
      template <size_t N>
      constexpr auto SIMPLE_LOADING =
          [](compilation_unit&,
             builtin_function::parameters_t,
             const ast::expr::call::arguments&) { UNREACHABLE(""); };
      template <>
      inline constexpr auto SIMPLE_LOADING<1> =
          [](compilation_unit& v,
             builtin_function::parameters_t param_t,
             const ast::expr::call::arguments& arg) {
            auto* to = load_parameter(
                v, param_t.at(0), arg.at(0), v.regs.parameters.at(0));
            return std::vector<operand*>{to};
          };
      template <>
      inline constexpr auto SIMPLE_LOADING<2> =
          [](compilation_unit& v,
             builtin_function::parameters_t params,
             const ast::expr::call::arguments& args) {
            auto* to = load_parameter(
                v, params.at(0), args.at(0), v.regs.parameters.at(0));
            auto* from = load_parameter(
                v, params.at(1), args.at(1), v.regs.parameters.at(1));
            return std::vector<operand*>{to, from};
          };
    } // namespace preprocessors
    namespace bodies {
      constexpr auto EXECUTE_MOVE =
          [](compilation_unit& v,
             const std::vector<operand*>& regs) -> operand* {
        return v.move(regs[0], regs[1]);
      };

      template <_instruction_t Ins, size_t N = instruction_t(Ins).n_params>
      constexpr auto EXECUTE_INSTRUC =
          [](compilation_unit&, const std::vector<operand*>&) -> operand* {
        UNREACHABLE("AAAAA");
      };
      template <_instruction_t Ins>
        requires(instruction_t(Ins).n_params == 1)
      constexpr auto EXECUTE_INSTRUC<Ins, 1> =
          [](compilation_unit& v, std::vector<operand*> regs) -> operand* {
            static_assert(instruction_t(Ins).n_params == 1);
            auto* param = regs[0];
            v.instruction(instruction_t(Ins), param);
            return param;
          };
      template <_instruction_t Ins>
        requires(instruction_t(Ins).n_params == 2)
      constexpr auto EXECUTE_INSTRUC<Ins, 2> =
          [](compilation_unit& v,
             const std::vector<operand*>& regs) -> operand* {
            static_assert(instruction_t(Ins).n_params == 2);
            auto* param = regs[0];
            v.instruction(instruction_t(Ins), param, regs[1]);
            return param;
          };
      template <_instruction_t Ins>
      constexpr auto POST_UNARY =
          [](compilation_unit& v,
             const std::vector<operand*>& args) -> operand* {
        auto* arg = args[0];
        auto* saved_before =
            v.move(v.regs.get(assembly::registers::ACCUMULATOR), arg);
        if (const auto* var = arg->variable()) {
          v.save_variable(var, arg);
          return saved_before;
        }
        UNREACHABLE("aaaa");
      };

      constexpr auto EXIT = [](compilation_unit& v,
                               std::vector<operand*> arg) -> operand* {
        v.current_phase = Phase::EXITING;
        v.move(v.regs.parameters.at(0), arg[0]);
        v.jump("exit");
        return nullptr;
      };
    } // namespace bodies
    namespace postprocessors {
      constexpr auto SIMPLE_RET    = [](compilation_unit&,
                                     operand* res) -> operand* { return res; };
      constexpr auto SAVE_VARIABLE = [](compilation_unit& v,
                                        operand* res) -> operand* {
        if (const auto* var = res->variable()) {
          v.save_variable(var, res);
          return res;
        }
        UNREACHABLE("aaaa");
      };

      static_assert(std::is_assignable_v<builtin_function::postprocess_t,
                                         decltype(SIMPLE_RET)>);
    }; // namespace postprocessors
  }; // namespace function
  constexpr void provider::create_builtin_function(
      cv_type ret,
      std::string name,
      const std::vector<cv_type>& args,
      builtin_function::preprocess_t pre,
      builtin_function::body_t body,
      builtin_function::postprocess_t post) {
    auto mangled = mangled_name::builtin_function(std::move(name), args);
    table.m_functions.emplace_builtin(
        mangled,
        ret,
        args,
        {std::move(pre), std::move(body), std::move(post)},
        false);
  }
  constexpr void provider::create_builtin_operator(
      cv_type ret,
      const operator_t& op,
      const std::vector<cv_type>& args,
      builtin_function::preprocess_t pre,
      builtin_function::body_t body,
      builtin_function::postprocess_t post) {
    create_builtin_function(ret,
                            op.caller_function(),
                            args,
                            std::move(pre),
                            std::move(body),
                            std::move(post));
  }
  template <_instruction_t Ins, size_t N>
  constexpr void provider::create_simple_operator(const operator_t& op,
                                                  cv_type type) {
    if constexpr (N == 1) {
      create_builtin_operator(type,
                              op,
                              {type},
                              function::preprocessors::SIMPLE_LOADING<N>,
                              function::bodies::EXECUTE_INSTRUC<Ins>,
                              function::postprocessors::SIMPLE_RET);
    } else if constexpr (N == 2) {
      create_builtin_operator(type,
                              op,
                              {type, type},
                              function::preprocessors::SIMPLE_LOADING<N>,
                              function::bodies::EXECUTE_INSTRUC<Ins>,
                              function::postprocessors::SIMPLE_RET);
    }
  }

  constexpr void provider::provide_operators() {
    create_builtin_operator(types::INTREF_T,
                            operator_t::assign,
                            {types::INTREF_T, types::INT_T},
                            function::preprocessors::SIMPLE_LOADING<2>,
                            function::bodies::EXECUTE_MOVE,
                            function::postprocessors::SIMPLE_RET);

    create_builtin_operator(
        types::INTREF_T,
        operator_t::pre_inc,
        {types::INTREF_T},
        function::preprocessors::SIMPLE_LOADING<1>,
        function::bodies::EXECUTE_INSTRUC<_instruction_t::inc>,
        function::postprocessors::SIMPLE_RET);
    create_builtin_operator(
        types::INTREF_T,
        operator_t::pre_dec,
        {types::INTREF_T},
        function::preprocessors::SIMPLE_LOADING<1>,
        function::bodies::EXECUTE_INSTRUC<_instruction_t::dec>,
        function::postprocessors::SIMPLE_RET);
    create_builtin_operator(types::INTREF_T,
                            operator_t::post_dec,
                            {types::INTREF_T},
                            function::preprocessors::SIMPLE_LOADING<1>,
                            function::bodies::POST_UNARY<_instruction_t::dec>,
                            function::postprocessors::SIMPLE_RET);
    create_builtin_operator(types::INTREF_T,
                            operator_t::post_inc,
                            {types::INTREF_T},
                            function::preprocessors::SIMPLE_LOADING<1>,
                            function::bodies::POST_UNARY<_instruction_t::inc>,
                            function::postprocessors::SIMPLE_RET);

    create_simple_operator<instruction_t::add>(operator_t::plus, types::INT_T);
    create_simple_operator<instruction_t::sub>(operator_t::minus, types::INT_T);
    create_simple_operator<instruction_t::mul>(operator_t::star, types::INT_T);
    create_simple_operator<instruction_t::div>(operator_t::fslash,
                                               types::INT_T);
    create_simple_operator<instruction_t::cmp>(operator_t::eq, types::INT_T);
    create_simple_operator<instruction_t::cmp>(operator_t::neq, types::INT_T);
    create_simple_operator<instruction_t::cmp>(operator_t::le, types::INT_T);
    create_simple_operator<instruction_t::cmp>(operator_t::lt, types::INT_T);
    create_simple_operator<instruction_t::cmp>(operator_t::ge, types::INT_T);
    create_simple_operator<instruction_t::cmp>(operator_t::gt, types::INT_T);
    create_simple_operator<instruction_t::and_>(operator_t::and_, types::INT_T);
    create_simple_operator<instruction_t::or_>(operator_t::or_, types::INT_T);
    create_simple_operator<instruction_t::not_>(operator_t::not_, types::INT_T);
    create_simple_operator<instruction_t::not_>(operator_t::star, types::INT_T);
    create_simple_operator<instruction_t::not_>(operator_t::ampersand,
                                                types::INT_T);
  }

  constexpr void provider::provide_functions() {
    create_builtin_function(types::VOID_T,
                            "exit",
                            {types::UINT_T},
                            function::preprocessors::SIMPLE_LOADING<1>,
                            function::bodies::EXIT,
                            function::postprocessors::SIMPLE_RET);
  }
  constexpr void provider::provide() {
    provide_operators();
    provide_functions();
  }

} // namespace builtin
} // namespace cmm::ir
