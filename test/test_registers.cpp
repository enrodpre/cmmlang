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
    regs = std::make_unique<assembly::registers>();
    var  = std::make_unique<decl::variable>(BOOL_T, identifier("a"), nullptr);
  }
  void TearDown() override {
    regs.reset();
    var.reset();
  }

  std::unique_ptr<registers> regs;
  std::unique_ptr<decl::variable> var;
};

class parameters_test : public registers_test {};

TEST_F(parameters_test, basic) {
  auto* reg = regs->get(registers::ACCUMULATOR);
  EXPECT_FALSE(!reg->empty());
  reg->hold_symbol(var.get());
  EXPECT_TRUE(reg->empty());
  reg->reset();
  EXPECT_FALSE(reg->empty());
}

#define IS_WRITTABLE(REG) !regs->get(registers::REG)->empty()

TEST_F(parameters_test, initial_state) {
  // All registers should be initially available
  EXPECT_TRUE(IS_WRITTABLE(SYSCALL_1));
  EXPECT_TRUE(IS_WRITTABLE(SYSCALL_2));
  EXPECT_TRUE(IS_WRITTABLE(SCRATCH_1));
  EXPECT_TRUE(IS_WRITTABLE(SCRATCH_4));
  EXPECT_TRUE(IS_WRITTABLE(SCRATCH_2));
  EXPECT_TRUE(IS_WRITTABLE(SCRATCH_3));
}
#define REGISTER(NUMBER) registers::to_realname(registers::m_parameters[NUMBER])

#define REG_EQ(REG, TYPE)                                         \
  EXPECT_EQ(REG, regs->get(registers::TYPE));                     \
  EXPECT_EQ(REG->string(), regs->get(registers::TYPE)->string());

#define NEXT_AND_FILL() transaction.next()->hold_symbol(var.get())

TEST_F(parameters_test, order_correctness) {
  auto transaction = regs->parameters();

  // All registers should be initially available
  REG_EQ(NEXT_AND_FILL(), SYSCALL_1);
  REG_EQ(NEXT_AND_FILL(), SYSCALL_2);
  REG_EQ(NEXT_AND_FILL(), SCRATCH_1);
  REG_EQ(NEXT_AND_FILL(), SCRATCH_4);
  REG_EQ(NEXT_AND_FILL(), SCRATCH_2);
  REG_EQ(NEXT_AND_FILL(), SCRATCH_3);
}

TEST_F(parameters_test, SingleTransactionBasicAllocation) {
  auto transaction = regs->parameters();

  reg* reg1        = transaction.next();
  REG_EQ(reg1, SYSCALL_1);
  EXPECT_FALSE(reg1->empty());
  reg1->hold_symbol(var.get());
  EXPECT_TRUE(reg1->empty());

  reg* reg2 = transaction.next();
  REG_EQ(reg2, SYSCALL_2);
  EXPECT_FALSE(reg2->empty());
}

#define RETRIEVE_AND_FILL(PARAMS, INT, STRING)     \
  auto CONCAT(param, INT) = PARAMS.next();         \
  EXPECT_EQ(STRING, CONCAT(param, INT)->format()); \
  CONCAT(param, INT)->hold_symbol(var.get())

// TEST_F(parameters_test, registers) {
//
//   EXPECT_EQ("r11", regs->get(registers::registers_t::AUX)->format());
//
//   auto params = regs->parameters();
//   RETRIEVE_AND_FILL(params, 0, REGISTER(0));
//   RETRIEVE_AND_FILL(params, 1, REGISTER(1));
//   auto params2 = regs->parameters();
//   RETRIEVE_AND_FILL(params2, 2, REGISTER(2));
//   params.reset();
//   RETRIEVE_AND_FILL(params, 3, REGISTER(0));
//   RETRIEVE_AND_FILL(params, 4, REGISTER(1));
//   {
//     auto params3 = regs->parameters();
//     RETRIEVE_AND_FILL(params3, 5, REGISTER(3));
//     auto params4 = regs->parameters();
//     RETRIEVE_AND_FILL(params4, 6, REGISTER(4));
//     params3.reset();
//     RETRIEVE_AND_FILL(params3, 7, REGISTER(3));
//   }
//   RETRIEVE_AND_FILL(params, 8, REGISTER(3));
// }

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
  EXPECT_FALSE(regs->get(registers::SYSCALL_1)->empty());
  EXPECT_FALSE(regs->get(registers::SYSCALL_2)->empty());
  EXPECT_FALSE(regs->get(registers::SCRATCH_1)->empty());
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
  REG_EQ(NEXT_AND_FILL(), SYSCALL_1);
  REG_EQ(NEXT_AND_FILL(), SYSCALL_2);
  REG_EQ(NEXT_AND_FILL(), SCRATCH_1);

  // Transaction2 should get the next available registers
  REG_EQ(transaction2.next()->hold_symbol(var.get()), SCRATCH_2);
  REG_EQ(transaction2.next()->hold_symbol(var.get()), SCRATCH_3);
  REG_EQ(transaction2.next()->hold_symbol(var.get()), SCRATCH_4);

  EXPECT_EQ(regs->available_parameters(), 0);
}

TEST_F(parameters_test, MultipleTransactionInterleaved) {
  auto transaction1 = regs->parameters();
  auto transaction2 = regs->parameters();
  auto transaction3 = regs->parameters();

  // Interleaved allocation
  REG_EQ(transaction1.next()->hold_symbol(var.get()), SYSCALL_1); // T1: SYSCALL_1
  REG_EQ(transaction2.next()->hold_symbol(var.get()), SYSCALL_2); // T2: SYSCALL_2
  REG_EQ(transaction3.next()->hold_symbol(var.get()), SCRATCH_1); // T3: SCRATCH_1
  REG_EQ(transaction1.next()->hold_symbol(var.get()), SCRATCH_4); // T1: SCRATCH_4
  REG_EQ(transaction2.next()->hold_symbol(var.get()), SCRATCH_2); // T2: SCRATCH_2
  REG_EQ(transaction3.next()->hold_symbol(var.get()), SCRATCH_3); // T3: SCRATCH_3

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
  REG_EQ(transaction2.next()->hold_symbol(var.get()), SYSCALL_1); // Now available again
  REG_EQ(transaction2.next()->hold_symbol(var.get()), SYSCALL_2); // Now available again
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

TEST_F(registers_test, find_variable) {
  const auto& opt = regs->find_var(var->ident);
  EXPECT_FALSE(opt);
  EXPECT_TRUE(IS_WRITTABLE(SCRATCH_3));

  auto* scratch3 = regs->get(registers::SCRATCH_3);
  scratch3->hold_symbol(var.get());
  EXPECT_FALSE(IS_WRITTABLE(SCRATCH_3));

  const auto& opt2 = regs->find_var(var->ident);
  EXPECT_TRUE(opt2);
  EXPECT_EQ(opt2, scratch3);
  EXPECT_FALSE(IS_WRITTABLE(SCRATCH_3));

  opt2.value()->reset();
  const auto& opt3 = regs->find_var(var->ident);
  EXPECT_FALSE(opt3);
  EXPECT_TRUE(IS_WRITTABLE(SCRATCH_3));
}
