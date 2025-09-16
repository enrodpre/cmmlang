#pragma once

#include "asm.hpp"

#include <array>
#include <cstdint>
#include <format>
#include <magic_enum/magic_enum.hpp>
#include <magic_enum/magic_enum_utility.hpp>
#include <string>
#include <utility>

namespace cmm::assembly {

template <typename... Args>
comment_block asmgen::begin_comment_block(std::format_string<Args...> std, Args&&... args) {
  std::string comment = std::format(std, std::forward<Args>(args)...);
  m_comment_blocks.emplace_back(comment);
  write_comment(comment);
  return {*this, comment};
}

enum class syscall_t : uint8_t;
struct label;
struct label_literal;
struct label_memory;
struct reg;
struct stack_memory;

[[nodiscard]] constexpr const syscall_data::properties_map& syscall_data::properties_array() {
  using enum syscall_t;
  static properties_map MAP{{{{READ, 3, 0}, {WRITE, 3, 1}, {EXIT, 1, 60}}}};
  return MAP;
}

constexpr std::string registers::to_realname(register_t r) {
  using enum register_t;
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
    default:
      return "r10";
  }
}

[[nodiscard]] constexpr reg* registers::parameter_at(const int i_) const {
  auto* reg_ = get(m_parameters.at(i_));
  if (!reg_->empty()) {
    REGISTER_WARN("Overwriting not writtable register {}", reg_->string());
  }
  reg_->reset();
  return reg_;
}

registers::registers()
    : m_registers(magic_enum::enum_for_each<register_t>(
          [](register_t t_val) { return std::make_unique<reg>(to_realname(t_val)); })) {}

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
void asmgen::write_instruction(const instruction_t& ins, Args&&... args) {
  assert(m_current_procedure);
  instruction_data data(ins);
  if constexpr ((sizeof...(Args) == 0)) {
    m_current_procedure->second += std::format("  {}\n", data.string());
  } else if constexpr ((sizeof...(Args) == 1)) {
    m_current_procedure->second +=
        std::format("  {} {}\n", data.string(), std::forward<Args>(args)...);
  } else if constexpr ((sizeof...(Args) == 2)) {
    m_current_procedure->second +=
        std::format("  {} {}, {}\n", data.string(), std::forward<Args>(args)...);
  }
}

} // namespace cmm::assembly
