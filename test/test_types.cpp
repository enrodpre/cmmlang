#include "types.hpp"
#include <gtest/gtest.h>

using namespace cmm;

struct types_test : public ::testing::Test {
  static constexpr cr_type create_type(type_category_t c) { return type::create(c); }

protected:
  cr_type boolean = create_type(type_category_t::bool_t);
};

TEST_F(types_test, tree) {
  EXPECT_TRUE(types::belongs_to(boolean, type_category_t::any_t));
  EXPECT_TRUE(types::belongs_to(boolean, type_category_t::fundamental_t));
  EXPECT_TRUE(types::belongs_to(boolean, type_category_t::bool_t));
  EXPECT_FALSE(types::belongs_to(boolean, type_category_t::compound_t));
}
