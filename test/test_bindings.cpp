#include "ast.hpp"
#include "lang.hpp"
#include "types.hpp"
#include <gtest/gtest.h>

using namespace cmm;
using namespace types;

struct binding_test : public ::testing::Test {
  manager& global   = manager::instance();
  type_id int_t     = SINT_T;
  type_id intref_t  = SINTREF_T;
  type_id intcref_t = add_const(SINTREF_T);
  type_id intrref_t = add_rvalue_reference(SINT_T);
};

using ast::translation_unit;

#define EXPECT_BINDABLE(CAT, PARAM, MODE)                                                      \
  EXPECT_TRUE(translation_unit::is_bindable_to(value_category_t::CAT, PARAM));                 \
  EXPECT_NO_THROW(translation_unit::bind_value(value_category_t::CAT, PARAM));                 \
  EXPECT_EQ(binding_mode_t::MODE, translation_unit::bind_value(value_category_t::CAT, PARAM));

#define EXPECT_NOT_BINDABLE(CAT, PARAM)                                                        \
  EXPECT_FALSE(translation_unit::is_bindable_to(value_category_t::CAT, PARAM));                \
  EXPECT_THROW(translation_unit::bind_value(value_category_t::CAT, PARAM), compilation_error);

TEST_F(binding_test, sanity_checks) {
  EXPECT_TRUE(is_rvalue(intrref_t).ok);
  EXPECT_TRUE(is_direct(int_t).ok);
  EXPECT_FALSE(is_direct(intref_t).ok);
  EXPECT_TRUE(is_lvalue(intref_t).ok);
  EXPECT_TRUE(is_lvalue(intcref_t).ok);
  EXPECT_TRUE(is_const(intcref_t).ok);
}
TEST_F(binding_test, LvalueToIntByValue) { EXPECT_BINDABLE(LVALUE, int_t, COPY); }

TEST_F(binding_test, LvalueToLvalueRef) { EXPECT_BINDABLE(LVALUE, intref_t, DIRECT); }

TEST_F(binding_test, LvalueToRvalueRefNotAllowed) { EXPECT_NOT_BINDABLE(LVALUE, intrref_t); }

TEST_F(binding_test, PrvalueToValue) { EXPECT_BINDABLE(PRVALUE, int_t, DIRECT); }

TEST_F(binding_test, PrvalueToRvalueRef) { EXPECT_BINDABLE(PRVALUE, intrref_t, DIRECT); }

TEST_F(binding_test, PrvalueToLvalueRefNotAllowed) { EXPECT_NOT_BINDABLE(PRVALUE, intref_t); }

TEST_F(binding_test, PrvalueToConstLvalueRef) { EXPECT_BINDABLE(PRVALUE, intcref_t, TEMPORARY); }

TEST_F(binding_test, XvalueToRvalueRef) { EXPECT_BINDABLE(XVALUE, intrref_t, DIRECT); }

TEST_F(binding_test, XvalueToLvalueRefNotAllowed) { EXPECT_NOT_BINDABLE(XVALUE, intref_t); }

TEST_F(binding_test, XvalueToConstLvalueRef) { EXPECT_BINDABLE(XVALUE, intcref_t, TEMPORARY); }
