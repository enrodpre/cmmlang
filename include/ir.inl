#pragma once

#include <format>

#include <magic_enum/magic_enum.hpp>
#include <tuple>
#include <utility>

#include "asm.hpp"
#include "common.hpp"
#include "ir.hpp"

namespace cmm::ir {

static_assert(std::formattable<const instruction_t&, char>);

template <assembly::Operand... Args>
void compilation_unit::instruction(const instruction_t& ins, Args&&... args) {

  if constexpr ((sizeof...(Args) == 0)) {
    asmgen.write_instruction(ins);
  } else if constexpr ((sizeof...(Args) == 1)) {

    auto op   = std::get<0>(std::forward_as_tuple(args...));
    auto data = instruction_data(ins);
    if (!data.can_address_memory) {
      // Use an aux register to load the value and then put it back onto the
      // address
      REGISTER_WARN("{} cannot address memory", data);
      static_assert(std::formattable<cmm::instruction_t, char>);
      if (!op->empty() &&
          op->content()->attribute == operand::symbol_container::symbol_attr::ADDRESS) {
        auto* aux = regs.get(assembly::register_t::AUX);
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
} // namespace cmm::ir
