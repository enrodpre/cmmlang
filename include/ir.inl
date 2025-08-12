#pragma once

#include "asm.hpp"
#include "common.hpp"
#include "lang.hpp"
#include "types.hpp"
#include <libassert/assert.hpp>
#include <magic_enum/magic_enum.hpp>
#include <tuple>
#include <utility>

namespace cmm::ir {

static_assert(std::formattable<const instruction_t&, char>);

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

    // const body_t CAST_TO_BOOL = [](compilation_unit& v,
    //                                const std::vector<operand*>& args) -> operand* {
    //   auto* arg = args[0];
    //   auto* aux = v.regs.parameters.next();
    //   v.move_immediate(aux, std::to_string(assembly::bits::masks::TO_BOOL));
    //   v.instruction(instruction_t::and_, arg, aux);
    //   return arg;
    // };

  }; // namespace function
}; // namespace builtin
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
} // namespace cmm::ir
// constexpr void builtin::provider::create_direct_conversion(cr_type from,
//                                                            cr_type to,
//                                                            const ir::function::body_t& b) {
//   table.m_conversions.emplace_direct(b, from, to);
// }
// constexpr void builtin::provider::create_glob_conversion(
//     std::string&& mangled,
//     const glob_conversion_function::condition_t& cond,
//     const glob_conversion_function::extractor_t& extr,
//     const ir::function::body_t& b) {
//   table.m_conversions.emplace_glob(std::move(mangled), b, cond, extr);
// }

// constexpr void builtin::provider::provide_conversions() {
//   create_glob_conversion("conv_lvalue_type",
//                          conversions::conditions::BELONGS_TO<type_category_t::lvalue_ref_t>,
//                          conversions::extractors::EXTRACT_TYPE,
//                          function::bodies::IDENTITY);
//   create_glob_conversion("conv_type_lvalue",
//                          conversions::conditions::BELONGS_TO<type_category_t::fundamental_t>,
//                          conversions::extractors::WRAP_REF,
//                          function::bodies::IDENTITY);
//   create_glob_conversion("conv_fundamental_bool",
//                          conversions::conditions::BELONGS_TO<type_category_t::fundamental_t>,
//                          conversions::extractors::GENERATE_TYPE<type_category_t::bool_t>,
//                          function::bodies::CAST_TO_BOOL);
//
//   create_direct_conversion(*UINT_T, *SINT_T, function::bodies::IDENTITY);
//   create_direct_conversion(*SINT_T, *UINT_T, function::bodies::IDENTITY);
// }
