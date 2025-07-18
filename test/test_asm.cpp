#include "asm.hpp"
#include <gtest/gtest.h>

using namespace cmm::assembly;

TEST(test_asm, operands) {
  auto* op = cmm::assembly::operand_factory::instance().create<stack_memory>(10);

  EXPECT_EQ("[rsp + 80]", op->value());
}

TEST(test_asm, registers) {
  registers regs;

  EXPECT_EQ("r11", regs.get(registers::registers_t::AUX)->name());
  EXPECT_EQ("rdi", regs.parameters.next()->name());
  EXPECT_EQ("rsi", regs.parameters.next()->name());
  EXPECT_EQ("rdx", regs.parameters.next()->name());
  EXPECT_EQ("r10", regs.parameters.next()->name());
  EXPECT_EQ("r8", regs.parameters.next()->name());
  EXPECT_EQ("r9", regs.parameters.next()->name());
  regs.parameters.reset();
  EXPECT_EQ("rdi", regs.parameters.next()->name());
}
