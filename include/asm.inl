#pragma once

#include "asm.hpp"
#include <magic_enum/magic_enum.hpp>

namespace cmm::assembly {

constexpr std::string registers::to_realname(registers_t r) {
  switch (r) {
    case RSP:
      return "rsp";
    case RBP:
      return "rbp";
    case ACCUMULATOR:
      return "rax";
    case COUNTER:
      return "rcx";
    case AUX:
      return "r11";
    case SYSCALL_1:
      return "rdi";
    case SYSCALL_2:
      return "rsi";
    case SCRATCH_1:
      return "rdx";
    case SCRATCH_2:
      return "r8";
    case SCRATCH_3:
      return "r9";
    case SCRATCH_4:
      return "r10";
    default:
      UNREACHABLE("not register ok");
  }
}

constexpr reg* registers::parameters_t::next() {
  return at(i++);
}

constexpr void registers::parameters_t::reset() {
  i = 0;
  for (int i = 0; i < 6; ++i) {
    at(i)->release();
  }
}
[[nodiscard]] constexpr reg* registers::parameters_t::at(size_t i_) const {
  auto* reg_ = regs.get(
      magic_enum::enum_cast<registers::registers_t>(m_parameters.at(i_))
          .value());
  reg_->release();
  return reg_;
}
namespace {
  constexpr reg* create_register(int i) {

    auto idx         = magic_enum::enum_cast<registers::registers_t>(i);
    std::string name = registers::to_realname(idx.value());
    return operand_factory::instance().create<reg>(name);
  }
}; // namespace

registers::store_type registers::initialize_registers() {
  store_type res = {create_register(0),
                    create_register(1),
                    create_register(2),
                    create_register(3),
                    create_register(4),
                    create_register(5),
                    create_register(6),
                    create_register(7),
                    create_register(8),
                    create_register(9),
                    create_register(10)};

  return res;
}
template <typename V, typename... Args>
  requires std::is_constructible_v<V, Args...>
V* operand_factory::create(Args&&... args) {
  return m_allocator.emplace<V>(std::forward<Args>(args)...);
}

namespace {
  template <typename... Args>
    requires(sizeof...(Args) == 0)
  constexpr std::string format_ins(instruction_t ins, Args&&...) {
    return std::format("{}", ins);
  }
  template <typename... Args>
    requires(sizeof...(Args) == 1)
  constexpr std::string format_ins(instruction_t ins, Args&&... args) {
    return std::format("{} {}", ins, std::forward<Args>(args)...);
  }
  template <typename... Args>
    requires(sizeof...(Args) == 2)
  constexpr std::string format_ins(instruction_t ins, Args&&... args) {
    return std::format("{} {}, {}", ins, std::forward<Args>(args)...);
  }
} // namespace

template <typename... Args>
void asmgen::write_instruction(const instruction_t& ins, Args&&... args) {}

} // namespace cmm::assembly
