
#include "test_base.hpp"
#include "types.hpp"
#include <gtest/gtest.h>

using namespace cmm;
using namespace types;

struct modifiers_test : public ::testing::Test {
  // static constexpr type_id create_type(core_t c) { return std::make_shared<type>(c); }

protected:
  manager& global  = manager::instance();
  core_t boolean_t = core_t::bool_t;
  type_id boolean  = global.make(core_t::bool_t);
  type_id cboolean = global.make(core_t::bool_t, {}, types::cv_qualification_t::CONST);
  type_id rboolean = make(core_t::bool_t, {layer_t::lvalue_ref_t});
  type_id float_t  = global.make(core_t::float_t);
};

TEST_F(modifiers_test, add_layer) {
  auto bool_ref = add_lvalue_reference(boolean);
  EXPECT_TRUE(is_lvalue(bool_ref).ok);
  EXPECT_FALSE(is_category<core_t::bool_t>(bool_ref).ok);
  EXPECT_TRUE(is_category<layer_t::lvalue_ref_t>(bool_ref).ok);
  EXPECT_FALSE(is_category<layer_t::rvalue_ref_t>(bool_ref).ok);
  EXPECT_TRUE(is_category<group_t::reference_t>(bool_ref).ok);
  EXPECT_FALSE(is_category<group_t::fundamental_t>(bool_ref).ok);

  auto bool_rvalue = add_rvalue_reference(boolean);
  EXPECT_TRUE(is_rvalue(bool_rvalue).ok);
  EXPECT_FALSE(bool_rvalue->layers.empty());
  EXPECT_EQ(layer_t::rvalue_ref_t, bool_rvalue->layers.top().tag);
  EXPECT_EQ(cv_qualification_t::NONE, bool_rvalue->layers.top().cv_qualifiers);
  EXPECT_EQ(cv_qualification_t::NONE, bool_rvalue->cvqual());
  EXPECT_EQ(layer_t::rvalue_ref_t, bool_rvalue->categorize());
  EXPECT_EQ(core_t::bool_t, bool_rvalue->core.kind);
}

TEST_F(modifiers_test, remove_layer) {
  EXPECT_EQ(boolean, remove_reference(boolean));
  EXPECT_EQ(boolean, remove_lvalue(boolean));
  EXPECT_EQ(boolean, remove_rvalue(boolean));
  EXPECT_EQ(boolean, remove_reference(rboolean));
  EXPECT_EQ(boolean, remove_lvalue(rboolean));
  EXPECT_NE(boolean, remove_rvalue(rboolean));
  EXPECT_EQ(rboolean, remove_rvalue(rboolean));
  EXPECT_EQ(cboolean, remove_reference(cboolean));
  EXPECT_EQ(boolean, remove_const(cboolean));
}

TEST_F(modifiers_test, add_cv) {
  auto before   = global.types_size();
  auto vboolean = add_volatile(boolean);
  EXPECT_NE(boolean, vboolean);
  EXPECT_EQ(before + 1, global.types_size());
  EXPECT_TRUE(is_volatile(vboolean).ok);

  EXPECT_EQ(cboolean, add_const(boolean));
}
TEST_F(modifiers_test, remove_cv) {}
