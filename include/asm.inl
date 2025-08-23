#pragma once

#include "asm.hpp"

#include <format>

#include <magic_enum/magic_enum.hpp>
#include <optional>
#include <string>
#include <type_traits>
#include <utility>

#include "allocator.hpp"

namespace cmm::assembly {
enum class syscall_t : uint8_t;
struct label;
struct label_literal;
struct label_memory;
struct reg;
struct stack_memory;
} // namespace cmm::assembly

namespace cmm::assembly {

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
  if (!reg_->is_writtable()) {
    REGISTER_WARN("Overwriting not writtable register {}", reg_->format());
  }
  reg_->release();
  return reg_;
}
namespace {
constexpr reg* create_register(int i) {

  auto idx         = magic_enum::enum_cast<register_t>(i);
  std::string name = registers::to_realname(idx.value());
  return operand_factory::create<reg>(name);
}
}; // namespace

constexpr registers::store_type registers::initialize_registers() {
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
  return ::cmm::memory::Allocator::instance().emplace<V>(std::forward<Args>(args)...);
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
void asmgen::write_instruction(const instruction_t& ins, Args&&... args) {
  instruction_data data(ins);
  if constexpr ((sizeof...(Args) == 0)) {
    m_text.write<2>("{}", data.string());
  } else if constexpr ((sizeof...(Args) == 1)) {
    m_text.write<2>("{} {}", data.string(), std::forward<Args>(args)...);
  } else if constexpr ((sizeof...(Args) == 2)) {
    m_text.write<2>("{} {}, {}", data.string(), std::forward<Args>(args)...);
  }
  m_text.write("\n");
}

} // namespace cmm::assembly
