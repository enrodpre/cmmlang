#include "ast.hpp"

#include <gtest/gtest.h>

#if 0
class StateTest : public CmmTest {
protected:
  ast::sequence<term::specifier> specs =
      prepare_specs({Specifier::int_t, Specifier::static_, Specifier::inline_});

  symbol_table* table;

  void SetUp() override { table = new symbol_table(); }
  void TearDown() override { delete table; }

  term::identifier create_ident(std::string&& id)
  {
    return {create_location(), std::move(id)};
  }

  void create_variable(const term::identifier& ident)
  {
    table->declare<variable>(ident, specs);
  }
  void declare_function(const term::identifier& ident)
  {
    table->declare<variable>(ident, specs);
  }
};

TEST_F(StateTest, functions)
{
  auto fn_id = create_ident("fn");
  EXPECT_FALSE(table->is_declared<function>(fn_id));

  // TODO put more checks
  declare_function(fn_id);

  EXPECT_TRUE(table->is_declared<function>(fn_id));
  const auto& regfunc = table->get<function>(fn_id);
  EXPECT_EQ(regfunc.identifier.value, "fn");
  EXPECT_EQ(regfunc.specs.type.type, Specifier::int_t);
  EXPECT_EQ(regfunc.specs.storage->type, Specifier::static_);
}

TEST_F(StateTest, variables)
{
  EXPECT_EQ(table->active_frame().active_scope().size(), 0);

  auto var_id = create_ident("a");
  create_variable(var_id);

  EXPECT_FALSE(table->get<variable>(var_id).defined());
  table->define_variable(var_id, 1);
  EXPECT_TRUE(table->get<variable>(var_id).defined());
  EXPECT_EQ(table->get<variable>(var_id).stack_location, 1);
  EXPECT_EQ(table->active_frame().active_scope().size(), 1);
  auto specs_a = table->get<variable>(var_id).specs;
  EXPECT_EQ(Specifier::int_t, specs_a.type.type);
  EXPECT_EQ(Specifier::static_, specs_a.storage->type);
  EXPECT_TRUE(specs_a.is_inline);
  EXPECT_FALSE(specs_a.is_constexpr);
}

struct CheckersTestData {
  symbol_table* table;
  ScopeType scope;
  bool expected_local, expected_global;
};

#endif
/* TEST(StateObjects, StateData) */
/* { */
/**/
/*   variables vars; */
/**/
/*   EXPECT_EQ(vars.Size(), 0); */
/**/
/*   vars.declarationare("undefined", {specs}); */
/**/
/*   EXPECT_EQ(vars.Size(), 1); */
/**/
/*   EXPECT_THROW(vars.Initialize("defined"), cmm:exceptionundeclared_symbol);
 */
/**/
/*   EXPECT_EQ(vars.Size(), 1); */
/**/
/*   size_t x  = 1; */
/**/
/*   size_t bv = 12, cv = 35; */
/*   vars.declarationare(std::string("a"), {specs, x}); */
/*   vars.declarationare("b", {specs, bv}); */
/*   vars.declarationare("c", {specs, cv}); */
/**/
/*   auto [id, v] = vars.Get("c"); */
/*   EXPECT_EQ(v, 35); */
/*   EXPECT_EQ(vars.Size(), 4); */
/**/
/*   auto it = vars.Get("b"); */
/*   EXPECT_EQ(*it.stack_loc, bv); */
/**/
/*   State::Get().stack_size = 13; */
/*   EXPECT_THROW(vars.Initialize("b"), cmm:exceptionAlreadyInitialized); */
/**/
/*   EXPECT_TRUE(vars.Get("b").IsInitialized()); */
/*   EXPECT_EQ(vars.Get("b").stack_loc, 12); */
/**/
/*   vars.Pop(); */
/*   EXPECT_EQ(vars.Size(), 3); */
/*   EXPECT_THROW(vars.declarationare("b", {specs}),
 * cmm:exceptionAlreadydeclarationared);
 */
/*   vars.Pop(); */
/*   EXPECT_EQ(vars.Size(), 2); */
/**/
/*   vars.declarationare("aad", {specs}); */
/*   EXPECT_THROW(vars.Check({.ident = "aad", .declared = true, .init = true}),
 */
/*                cmm:exceptionundefined_symbol); */
/* } */
