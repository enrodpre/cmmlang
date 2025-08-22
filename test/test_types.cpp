#include "types.hpp"
#include <gtest/gtest.h>

using namespace cmm;

struct types_test : public ::testing::Test {
  static constexpr crtype create_type(type_category_t c) { return type::create(c); }

protected:
  type_category_t boolean = type_category_t::bool_t;
};

TEST_F(types_test, tree) {
  EXPECT_TRUE(belongs_to(boolean, type_category_t::any_t));
  EXPECT_TRUE(belongs_to(boolean, type_category_t::fundamental_t));
  EXPECT_TRUE(belongs_to(boolean, type_category_t::bool_t));
  EXPECT_FALSE(belongs_to(boolean, type_category_t::compound_t));
}

TEST_F(types_test, comparisons) {
  const auto& t1 = type::create_fundamental(type_category_t::bool_t);
  const auto& t2 = type::create_fundamental(type_category_t::bool_t);

  EXPECT_EQ(t1, t1);
  EXPECT_EQ(t1, t2);

  const auto& t3 = type::create_fundamental(type_category_t::bool_t, true);

  EXPECT_NE(t1, t2);
}
