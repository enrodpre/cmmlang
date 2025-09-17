#include "asm.hpp"
#include "ast.hpp"
#include "types.hpp"
#include <gtest/gtest.h>
#include <magic_enum/magic_enum.hpp>
#include <memory>

using namespace cmm;
using namespace ast;
using namespace assembly;

class registers_test : public ::testing::Test {

protected:
  void SetUp() override {
    regs = &registers::instance();
    var  = std::make_unique<decl::variable>(BOOL_T, identifier("a"), nullptr);
  }
  void TearDown() override {
    regs->reset();
    var.reset();
  }

  registers* regs;
  std::unique_ptr<decl::variable> var;
};

using parameters_test = registers_test;

#define IS_EMPTY_REG(REG) regs->get(registers::REG)->empty()

TEST_F(parameters_test, basic) {
  auto* reg = regs->get(registers::ACCUMULATOR);
  EXPECT_TRUE(reg->empty());
  reg->hold_symbol(var.get());
  EXPECT_FALSE(reg->empty());
  reg->reset();
  EXPECT_TRUE(reg->empty());
}

TEST_F(parameters_test, initial_state) {
  // All registers should be initially available
  EXPECT_TRUE(IS_EMPTY_REG(SYSCALL_1));
  EXPECT_TRUE(IS_EMPTY_REG(SYSCALL_2));
  EXPECT_TRUE(IS_EMPTY_REG(SCRATCH_1));
  EXPECT_TRUE(IS_EMPTY_REG(SCRATCH_4));
  EXPECT_TRUE(IS_EMPTY_REG(SCRATCH_2));
  EXPECT_TRUE(IS_EMPTY_REG(SCRATCH_3));
}

#define REG_EQ(REG, TYPE)                                         \
  EXPECT_EQ(REG, regs->get(registers::TYPE));                     \
  EXPECT_EQ(REG->string(), regs->get(registers::TYPE)->string());

#define NEXT_AND_FILL() transaction.next()->hold_symbol(var.get())

#define T_FILL_AND_NEXT(TRANS, VAR, REG)                         \
  auto* VAR = TRANS.next();                                      \
  EXPECT_EQ(VAR, regs->get(registers::REG));                     \
  EXPECT_EQ(VAR->string(), regs->get(registers::REG)->string()); \
  VAR->hold_symbol(var.get());

#define FILL_AND_NEXT(VAR, REG)                                  \
  auto* VAR = transaction.next();                                \
  EXPECT_EQ(VAR, regs->get(registers::REG));                     \
  EXPECT_EQ(VAR->string(), regs->get(registers::REG)->string()); \
  VAR->hold_symbol(var.get());

TEST_F(parameters_test, order_correctness) {
  auto transaction = regs->parameters();

  // All registers should be initially available
  FILL_AND_NEXT(first, SYSCALL_1);
  FILL_AND_NEXT(second, SYSCALL_2);
  FILL_AND_NEXT(third, SCRATCH_1);
  FILL_AND_NEXT(fourth, SCRATCH_4);
  FILL_AND_NEXT(fifth, SCRATCH_2);
  FILL_AND_NEXT(sixth, SCRATCH_3);
}

TEST_F(parameters_test, SingleTransactionBasicAllocation) {
  auto transaction = regs->parameters();

  reg* reg1        = transaction.next();
  REG_EQ(reg1, SYSCALL_1);
  EXPECT_TRUE(reg1->empty());
  reg1->hold_symbol(var.get());
  EXPECT_FALSE(reg1->empty());

  reg* reg2 = transaction.next();
  REG_EQ(reg2, SYSCALL_2);
  EXPECT_TRUE(reg2->empty());
}

TEST_F(parameters_test, SingleTransactionExhaustAllRegisters) {
  auto transaction = regs->parameters();

  // Allocate all registers
  std::vector<registers::register_t> expected_order = {registers::SYSCALL_1,
                                                       registers::SYSCALL_2,
                                                       registers::SCRATCH_1,
                                                       registers::SCRATCH_4,
                                                       registers::SCRATCH_2,
                                                       registers::SCRATCH_3};

  for (registers::register_t r : expected_order) {
    auto* reg = NEXT_AND_FILL();
    EXPECT_EQ(reg->string(), regs->get(r)->string());
  }

  EXPECT_ANY_THROW(transaction.next());
}

TEST_F(parameters_test, TransactionManualReset) {
  auto transaction = regs->parameters();

  NEXT_AND_FILL(); // SYSCALL_1
  NEXT_AND_FILL(); // SYSCALL_2
  EXPECT_EQ(regs->available_parameters(), 4);

  transaction.reset();
  EXPECT_EQ(regs->available_parameters(), 6);
}

TEST_F(parameters_test, TransactionDestructorReset) {

  EXPECT_EQ(regs->available_parameters(), 6);

  {
    auto transaction = regs->parameters();
    NEXT_AND_FILL(); // SYSCALL_1
    NEXT_AND_FILL(); // SYSCALL_2
    NEXT_AND_FILL(); // SCRATCH_1
    EXPECT_EQ(regs->available_parameters(), 3);
  } // Transaction destructor should reset registers

  EXPECT_EQ(regs->available_parameters(), 6);
  EXPECT_TRUE(IS_EMPTY_REG(SYSCALL_1));
  EXPECT_TRUE(IS_EMPTY_REG(SYSCALL_2));
  EXPECT_TRUE(IS_EMPTY_REG(SCRATCH_1));
}

TEST_F(parameters_test, InactiveTransactionThrowsException) {
  auto transaction = regs->parameters();
  transaction.reset();
  EXPECT_NO_THROW(transaction.reset()); // Multiple resets should be safe
}

TEST_F(parameters_test, TwoSimultaneousTransactions) {
  auto transaction  = regs->parameters();
  auto transaction2 = regs->parameters();

  // Transaction1 allocates first 3 registers
  FILL_AND_NEXT(first, SYSCALL_1);
  FILL_AND_NEXT(second, SYSCALL_2);
  FILL_AND_NEXT(third, SCRATCH_1);

  // Transaction2 should get the next available registers
  T_FILL_AND_NEXT(transaction2, fourth, SCRATCH_4);
  T_FILL_AND_NEXT(transaction2, fifth, SCRATCH_2);
  T_FILL_AND_NEXT(transaction2, sixth, SCRATCH_3);

  EXPECT_EQ(regs->available_parameters(), 0);
}

TEST_F(parameters_test, MultipleTransactionInterleaved) {
  auto transaction1 = regs->parameters();
  auto transaction2 = regs->parameters();
  auto transaction3 = regs->parameters();

  // Interleaved allocation
  T_FILL_AND_NEXT(transaction1, first, SYSCALL_1);
  T_FILL_AND_NEXT(transaction2, second, SYSCALL_2);
  T_FILL_AND_NEXT(transaction3, third, SCRATCH_1);
  T_FILL_AND_NEXT(transaction1, forth, SCRATCH_4);
  T_FILL_AND_NEXT(transaction2, fifth, SCRATCH_2);
  T_FILL_AND_NEXT(transaction3, sixth, SCRATCH_3);

  // Verify final state
  EXPECT_EQ(regs->available_parameters(), 0);
}

TEST_F(parameters_test, TransactionResetAffectsOthers) {
  auto transaction1 = regs->parameters();
  auto transaction2 = regs->parameters();

  // Both transactions allocate registers
  transaction1.next()->hold_symbol(var.get()); // SYSCALL_1
  transaction1.next()->hold_symbol(var.get()); // SYSCALL_2
  transaction2.next()->hold_symbol(var.get()); // SCRATCH_1
  transaction2.next()->hold_symbol(var.get()); // SCRATCH_4

  EXPECT_EQ(regs->available_parameters(), 2);

  // Transaction1 resets - its registers become available
  transaction1.reset();
  EXPECT_EQ(regs->available_parameters(), 4);

  // Transaction2 should still work and can now access freed registers
  T_FILL_AND_NEXT(transaction2, first, SYSCALL_1);
  T_FILL_AND_NEXT(transaction2, second, SYSCALL_2);
}

TEST_F(parameters_test, ManyTransactionsSequentially) {
  constexpr int NUM_TRANSACTIONS = 10;

  for (int i = 0; i < NUM_TRANSACTIONS; ++i) {
    auto transaction = regs->parameters();

    // Each transaction should be able to allocate all registers
    for (int j = 0; j < 6; ++j) {
      EXPECT_NO_THROW(NEXT_AND_FILL());
    }

    EXPECT_ANY_THROW(transaction.next());
    EXPECT_EQ(regs->available_parameters(), 0);

    // Transaction destructor will reset at end of scope
  }

  // After all transactions, all registers should be available
  EXPECT_EQ(regs->available_parameters(), 6);
}

TEST_F(registers_test, sanity_check) {
  auto* reg = regs->get(registers::ACCUMULATOR);
  EXPECT_EQ(reg, reg);
  REG_EQ(reg, ACCUMULATOR);
  EXPECT_EQ(reg, reg->hold_value());
  REG_EQ(reg->hold_value(), ACCUMULATOR);
}
TEST_F(registers_test, find_variable) {
  const auto& opt = regs->find_var(var->ident);
  EXPECT_FALSE(opt);
  EXPECT_TRUE(IS_EMPTY_REG(SCRATCH_3));

  auto* scratch3 = regs->get(registers::SCRATCH_3);
  scratch3->hold_symbol(var.get());
  EXPECT_FALSE(IS_EMPTY_REG(SCRATCH_3));

  const auto& opt2 = regs->find_var(var->ident);
  EXPECT_TRUE(opt2);
  EXPECT_EQ(opt2, scratch3);
  EXPECT_FALSE(IS_EMPTY_REG(SCRATCH_3));

  opt2.value()->reset();
  const auto& opt3 = regs->find_var(var->ident);
  EXPECT_FALSE(opt3);
  EXPECT_TRUE(IS_EMPTY_REG(SCRATCH_3));
}
