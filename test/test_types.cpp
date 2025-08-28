#include "common.hpp"
#include "lang.hpp"
#include "test_base.hpp"
#include "types.hpp"
#include <gtest/gtest.h>
#include <memory>

using namespace cmm;

struct types_test : public ::testing::Test {
  static constexpr type create_type(category_t c) { return std::make_shared<type>(c); }

protected:
  category_t boolean_t = category_t::bool_t;
  type boolean             = type::create_fundamental(category_t::bool_t);
  type cboolean            = type::create_fundamental(category_t::bool_t, true);
};

TEST_F(types_test, tree) {
  EXPECT_TRUE(belongs_to(boolean_t, category_t::any_t));
  EXPECT_TRUE(belongs_to(boolean_t, category_t::fundamental_t));
  EXPECT_TRUE(belongs_to(boolean_t, category_t::bool_t));
  EXPECT_FALSE(belongs_to(boolean_t, category_t::compound_t));
}

TEST_F(types_test, comparisons) {
  EXPECT_TRUE(boolean->match(boolean));
  EXPECT_FALSE(cboolean->match(type::create_fundamental(category_t::bool_t)));
  EXPECT_FALSE(boolean->match(cboolean));
  EXPECT_TRUE(boolean->match(type::create_fundamental(category_t::bool_t)));
}
struct binding_test : public ::testing::Test {
  const type int_t     = type::create_fundamental(category_t::sint_t);
  const type intref_t  = type::create_lvalue(std::make_shared<const type>(*int_t));
  const type intcref_t = type::create_lvalue(std::make_shared<const type>(*int_t), true);
  const type intrref_t = type::create_rvalue(std::make_shared<const type>(*int_t));
};

TEST_F(binding_test, LvalueToIntByValue) {
  EXPECT_TRUE(is_bindeable(value_category_t::LVALUE, int_t, int_t));
  EXPECT_NOTHROW_AND_RETURN(LVALUE, int_t, int_t);
}

TEST_F(binding_test, LvalueToLvalueRef) {
  EXPECT_TRUE(is_bindeable(value_category_t::LVALUE, int_t, intref_t));
  EXPECT_NOTHROW_AND_RETURN(LVALUE, int_t, intref_t);
}

TEST_F(binding_test, LvalueToRvalueRefNotAllowed) {
  EXPECT_FALSE(is_bindeable(value_category_t::LVALUE, int_t, intrref_t));
  EXPECT_THROW(bind_argument(value_category_t::LVALUE, int_t, intrref_t), compilation_error);
}

TEST_F(binding_test, PrvalueToValue) {
  EXPECT_TRUE(is_bindeable(value_category_t::PRVALUE, int_t, int_t));
  EXPECT_NOTHROW_AND_RETURN(PRVALUE, int_t, intref_t);
}

TEST_F(binding_test, PrvalueToRvalueRef) {
  EXPECT_TRUE(is_bindeable(value_category_t::PRVALUE, int_t, intrref_t));
  EXPECT_NOTHROW_AND_RETURN(LVALUE, int_t, intrref_t);
}

TEST_F(binding_test, PrvalueToLvalueRefNotAllowed) {
  EXPECT_FALSE(is_bindeable(value_category_t::PRVALUE, int_t, intref_t));
  EXPECT_THROW(bind_argument(value_category_t::PRVALUE, int_t, intref_t), compilation_error);
}

TEST_F(binding_test, PrvalueToConstLvalueRef) {
  EXPECT_TRUE(is_bindeable(value_category_t::PRVALUE, int_t, intcref_t));
  EXPECT_NOTHROW_AND_RETURN(PRVALUE, int_t, intcref_t);
}

TEST_F(binding_test, XvalueToRvalueRef) {
  EXPECT_TRUE(is_bindeable(value_category_t::XVALUE, int_t, intrref_t));
  EXPECT_NOTHROW_AND_RETURN(XVALUE, int_t, intrref_t);
}

TEST_F(binding_test, XvalueToLvalueRefNotAllowed) {
  EXPECT_FALSE(is_bindeable(value_category_t::XVALUE, int_t, intref_t));
}

TEST_F(binding_test, XvalueToConstLvalueRef) {
  EXPECT_TRUE(is_bindeable(value_category_t::XVALUE, int_t, intcref_t));
}
