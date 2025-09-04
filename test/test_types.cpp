#include "common.hpp"
#include "test_base.hpp"
#include "types.hpp"
#include <gtest/gtest.h>

using namespace cmm;
using namespace types;

struct types_test : public ::testing::Test {
  // static constexpr type_id create_type(core_t c) { return std::make_shared<type>(c); }

protected:
  manager& global  = manager::instance();
  core_t boolean_t = core_t::bool_t;
  type_id boolean  = global.make(core_t::bool_t);
  type_id cboolean = global.make(core_t::bool_t, {}, types::cv_qualification_t::CONST);
  type_id rboolean = make(core_t::bool_t, {layer_t::lvalue_ref_t});
  type_id float_t  = global.make(core_t::float_t);
};

TEST_F(types_test, tree) {
  EXPECT_TRUE(belongs_to(boolean_t, group_t::any_t));
  EXPECT_TRUE(belongs_to(boolean_t, group_t::fundamental_t));
  EXPECT_TRUE(belongs_to(boolean_t, core_t::bool_t));
  EXPECT_FALSE(belongs_to(boolean_t, group_t::compound_t));
}

// static_assert(std::is_pointer_v<const bool>);

TEST_F(types_test, creation) {
  auto before  = global.types_size();
  type_id sint = global.make(core_t::sint_t);
  EXPECT_EQ(before + 1, global.types_size());
  type_id uint = global.make(core_t::uint_t);
  EXPECT_EQ(sint.value() + 1, uint.value());
}

TEST_F(types_test, internal) {
  EXPECT_EQ(core_t::bool_t, boolean->categorize());
  EXPECT_EQ(core_t::bool_t, cboolean->categorize());
  EXPECT_TRUE(boolean->layers.empty());
  EXPECT_TRUE(cboolean->layers.empty());
  EXPECT_EQ(cv_qualification_t::NONE, boolean->cv_qualifiers);
  EXPECT_EQ(cv_qualification_t::NONE, boolean->cvqual());
  EXPECT_EQ(cv_qualification_t::CONST, cboolean->cv_qualifiers);
  EXPECT_EQ(cv_qualification_t::CONST, cboolean->cvqual());

  EXPECT_EQ(layer_t::lvalue_ref_t, rboolean->categorize());
  EXPECT_FALSE(rboolean->layers.empty());
  EXPECT_EQ(layer_t::lvalue_ref_t, rboolean->layers.top().tag);
  EXPECT_EQ(cv_qualification_t::NONE, rboolean->cvqual());
  EXPECT_EQ(cv_qualification_t::NONE, rboolean->layers.top().cv_qualifiers);
}

TEST_F(types_test, comparisons) {
  EXPECT_EQ(boolean, boolean);
  EXPECT_NE(cboolean, BASIC_T(bool_t));
  EXPECT_NE(boolean, cboolean);
  EXPECT_EQ(boolean, BASIC_T(bool_t));
}

TEST_F(types_test, matching) {
  EXPECT_TRUE(any(boolean).ok);
  EXPECT_TRUE(is_category<core_t::bool_t>(boolean).ok);
  EXPECT_FALSE(is_category<core_t::sint_t>(boolean).ok);
  EXPECT_TRUE(is_category<core_t::bool_t>(cboolean).ok);
  EXPECT_FALSE(is_category<core_t::sint_t>(cboolean).ok);
  EXPECT_FALSE(is_const(boolean).ok);
  EXPECT_TRUE(is_const(cboolean).ok);
  EXPECT_FALSE(is_volatile(boolean).ok);
  EXPECT_FALSE(is_volatile(cboolean).ok);
  EXPECT_TRUE(is_integral(boolean).ok);
  EXPECT_TRUE(is_integral(cboolean).ok);
  EXPECT_TRUE(is_arithmetic(boolean).ok);
  EXPECT_TRUE(is_arithmetic(cboolean).ok);
  EXPECT_FALSE(is_lvalue(boolean).ok);
  EXPECT_FALSE(is_lvalue(cboolean).ok);
  EXPECT_FALSE(is_floating(boolean).ok);
  EXPECT_FALSE(is_floating(cboolean).ok);
  EXPECT_FALSE(is_compound(boolean).ok);
  EXPECT_FALSE(is_compound(cboolean).ok);
  EXPECT_TRUE(is_direct(boolean).ok);
  EXPECT_FALSE(is_direct(rboolean).ok);
}

TEST_F(types_test, combining_matchers) {
  auto integral_or_lvalue = is_integral || is_lvalue;
  EXPECT_TRUE(integral_or_lvalue(boolean).ok);
  EXPECT_TRUE(integral_or_lvalue(rboolean).ok);
  EXPECT_FALSE(integral_or_lvalue(float_t).ok);

  auto integral_and_arithmetic = is_integral && is_arithmetic;
  EXPECT_TRUE(integral_and_arithmetic(boolean).ok);
  EXPECT_FALSE(integral_and_arithmetic(float_t).ok);

  auto not_lvalue = !is_lvalue;
  EXPECT_TRUE(not_lvalue(boolean).ok);
  EXPECT_FALSE(not_lvalue(rboolean).ok);

  auto not_integral_nor_arithmetic = !integral_and_arithmetic;
  EXPECT_FALSE(not_integral_nor_arithmetic(boolean).ok);
  EXPECT_TRUE(not_integral_nor_arithmetic(float_t).ok);
}

TEST_F(types_test, modification_internal) {
  EXPECT_TRUE(boolean->layers.empty());
  EXPECT_EQ(core_t::bool_t, boolean->categorize());
  EXPECT_EQ(cv_qualification_t::NONE, boolean->cvqual());
}

TEST_F(types_test, stringify) {
  EXPECT_EQ(boolean->string(), "bool");
  EXPECT_EQ(cboolean->string(), "boolc");
  EXPECT_EQ(rboolean->string(), "boolref");
  EXPECT_EQ(add_volatile(cboolean)->string(), "boolcv");
  EXPECT_EQ(add_const(rboolean)->string(), "boolcref");
}
