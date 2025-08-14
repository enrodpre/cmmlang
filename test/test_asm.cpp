#include "asm.hpp"
#include "ast.hpp"
#include "types.hpp"
#include <gtest/gtest.h>
#include <magic_enum/magic_enum.hpp>
#include <memory>

using namespace cmm;
using namespace assembly;
using register_n = assembly::registers::register_t;
using namespace ast;

class ParametersTest : public ::testing::Test {
protected:
  void SetUp() override {
    regs = std::make_unique<registers>();
    var  = std::make_unique<decl::variable>(cmm::type::create(type_category_t::bool_t), "a");
  }
  void TearDown() override {
    regs.reset();
    var.reset();
  }

  std::unique_ptr<registers> regs;
  std::unique_ptr<decl::variable> var;
};

TEST_F(ParametersTest, basic) {
  auto* reg = regs->get(registers::ACCUMULATOR);
  EXPECT_TRUE(reg->is_writtable());
  reg->hold_address(var.get());
  EXPECT_FALSE(reg->is_writtable());
  reg->release();
  EXPECT_TRUE(reg->is_writtable());
}

#define IS_WRITTABLE(REG) regs->get(registers::REG)->is_writtable()
TEST_F(ParametersTest, initial_state) {
  // All registers should be initially available
  EXPECT_TRUE(IS_WRITTABLE(SYSCALL_1));
  EXPECT_TRUE(IS_WRITTABLE(SYSCALL_2));
  EXPECT_TRUE(IS_WRITTABLE(SCRATCH_1));
  EXPECT_TRUE(IS_WRITTABLE(SCRATCH_4));
  EXPECT_TRUE(IS_WRITTABLE(SCRATCH_2));
  EXPECT_TRUE(IS_WRITTABLE(SCRATCH_3));
}
#define REGISTER(NUMBER) registers::to_realname(registers::m_parameters[NUMBER])

#define REG_EQ(REG, TYPE) EXPECT_EQ(REG->format(), regs->get(registers::TYPE)->format())
#define NEXT_AND_FILL()   transaction.next()->hold_value(var.get())

TEST_F(ParametersTest, order_correctness) {
  auto transaction = regs->parameters();

  // All registers should be initially available
  REG_EQ(NEXT_AND_FILL(), SYSCALL_1);
  REG_EQ(NEXT_AND_FILL(), SYSCALL_2);
  REG_EQ(NEXT_AND_FILL(), SCRATCH_1);
  REG_EQ(NEXT_AND_FILL(), SCRATCH_4);
  REG_EQ(NEXT_AND_FILL(), SCRATCH_2);
  REG_EQ(NEXT_AND_FILL(), SCRATCH_3);
}

TEST_F(ParametersTest, SingleTransactionBasicAllocation) {
  auto transaction = regs->parameters();

  reg* reg1        = transaction.next();
  REG_EQ(reg1, registers::SYSCALL_1);
  EXPECT_TRUE(reg1->is_writtable());
  reg1->hold_value(var.get());
  EXPECT_FALSE(reg1->is_writtable());

  reg* reg2 = transaction.next();
  REG_EQ(reg2, registers::SYSCALL_2);
  EXPECT_TRUE(reg2->is_writtable());
}

#define RETRIEVE_AND_FILL(PARAMS, INT, STRING) \
  auto CONCAT(param, INT) = PARAMS.next(); \
  EXPECT_EQ(STRING, CONCAT(param, INT)->format()); \
  CONCAT(param, INT)->hold_address(var.get())

// TEST_F(ParametersTest, registers) {
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

TEST_F(ParametersTest, SingleTransactionExhaustAllRegisters) {
  auto transaction = regs->parameters();

  // Allocate all registers
  std::vector<register_n> expected_order = {registers::SYSCALL_1,
                                            registers::SYSCALL_2,
                                            registers::SCRATCH_1,
                                            registers::SCRATCH_4,
                                            registers::SCRATCH_2,
                                            registers::SCRATCH_3};

  for (register_n r : expected_order) {
    auto* reg = NEXT_AND_FILL();
    EXPECT_EQ(reg->format(), regs->get(r)->format());
  }

  EXPECT_ANY_THROW(transaction.next());
}

TEST_F(ParametersTest, TransactionManualReset) {
  auto transaction = regs->parameters();

  NEXT_AND_FILL(); // SYSCALL_1
  NEXT_AND_FILL(); // SYSCALL_2
  EXPECT_EQ(regs->available_parameters(), 4);

  transaction.reset();
  EXPECT_EQ(regs->available_parameters(), 6);
}

TEST_F(ParametersTest, TransactionDestructorReset) {

  EXPECT_EQ(regs->available_parameters(), 6);

  {
    auto transaction = regs->parameters();
    NEXT_AND_FILL(); // SYSCALL_1
    NEXT_AND_FILL(); // SYSCALL_2
    NEXT_AND_FILL(); // SCRATCH_1
    EXPECT_EQ(regs->available_parameters(), 3);
  } // Transaction destructor should reset registers

  EXPECT_EQ(regs->available_parameters(), 6);
  EXPECT_TRUE(regs->get(registers::SYSCALL_1)->is_writtable());
  EXPECT_TRUE(regs->get(registers::SYSCALL_2)->is_writtable());
  EXPECT_TRUE(regs->get(registers::SCRATCH_1)->is_writtable());
}

TEST_F(ParametersTest, InactiveTransactionThrowsException) {
  auto transaction = regs->parameters();
  transaction.reset();
  EXPECT_NO_THROW(transaction.reset()); // Multiple resets should be safe
}

TEST_F(ParametersTest, TwoSimultaneousTransactions) {
  auto transaction  = regs->parameters();
  auto transaction2 = regs->parameters();

  // Transaction1 allocates first 3 registers
  REG_EQ(NEXT_AND_FILL(), registers::SYSCALL_1);
  REG_EQ(NEXT_AND_FILL(), registers::SYSCALL_2);
  REG_EQ(NEXT_AND_FILL(), registers::SCRATCH_1);

  // Transaction2 should get the next available registers
  REG_EQ(transaction2.next()->hold_value(var.get()), registers::SCRATCH_4);
  REG_EQ(transaction2.next()->hold_value(var.get()), registers::SCRATCH_2);
  REG_EQ(transaction2.next()->hold_value(var.get()), registers::SCRATCH_3);

  EXPECT_EQ(regs->available_parameters(), 0);
}

TEST_F(ParametersTest, MultipleTransactionInterleaved) {
  auto transaction1 = regs->parameters();
  auto transaction2 = regs->parameters();
  auto transaction3 = regs->parameters();

  // Interleaved allocation
  REG_EQ(transaction1.next()->hold_value(var.get()), registers::SYSCALL_1); // T1: SYSCALL_1
  REG_EQ(transaction2.next()->hold_value(var.get()), registers::SYSCALL_2); // T2: SYSCALL_2
  REG_EQ(transaction3.next()->hold_value(var.get()), registers::SCRATCH_1); // T3: SCRATCH_1
  REG_EQ(transaction1.next()->hold_value(var.get()), registers::SCRATCH_4); // T1: SCRATCH_4
  REG_EQ(transaction2.next()->hold_value(var.get()), registers::SCRATCH_2); // T2: SCRATCH_2
  REG_EQ(transaction3.next()->hold_value(var.get()), registers::SCRATCH_3); // T3: SCRATCH_3

  // Verify final state
  EXPECT_EQ(regs->available_parameters(), 0);
}

TEST_F(ParametersTest, TransactionResetAffectsOthers) {
  auto transaction1 = regs->parameters();
  auto transaction2 = regs->parameters();

  // Both transactions allocate registers
  transaction1.next()->hold_value(var.get()); // SYSCALL_1
  transaction1.next()->hold_value(var.get()); // SYSCALL_2
  transaction2.next()->hold_value(var.get()); // SCRATCH_1
  transaction2.next()->hold_value(var.get()); // SCRATCH_4

  EXPECT_EQ(regs->available_parameters(), 2);

  // Transaction1 resets - its registers become available
  transaction1.reset();
  EXPECT_EQ(regs->available_parameters(), 4);

  // Transaction2 should still work and can now access freed registers
  REG_EQ(transaction2.next()->hold_value(var.get()), registers::SYSCALL_1); // Now available again
  REG_EQ(transaction2.next()->hold_value(var.get()), registers::SYSCALL_2); // Now available again
}

TEST_F(ParametersTest, ManyTransactionsSequentially) {
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
