#include "ast/expr.hpp"
#include "ast/tree.hpp"
#include "token.hpp"
#include "types.hpp"
#include <gtest/gtest.h>
#include <memory>

using namespace cmm;
using namespace ast;

class function_test : public ::testing::Test {

protected:
  decl::specifiers sint_specs;
  expr::identifier expr1 = expr::identifier(token(token_t::ident, location(1, 2), "ident"));
  expr::literal expr2 =
      expr::literal(token(token_t::int_lit, location(1, 2), "10"), expr::literal_t::SINT);
  void SetUp() override {
    func = std::make_unique<decl::function>(
        decl::specifiers{SINT_T},
        token(token_t::ident, location(1, 2), "func"),
        std::vector{ast::decl::variable(SINT_T, nullptr, identifier("var1"), &expr1),
                    ast::decl::variable(UINT_T, nullptr, identifier("var2"), &expr2)},
        nullptr);
    var = std::make_unique<decl::variable>(BOOL_T, identifier("a"), nullptr);
  }
  void TearDown() override {
    func.reset();
    var.reset();
  }

  std::unique_ptr<ast::decl::function> func;
  std::unique_ptr<decl::variable> var;
};

TEST_F(function_test, basic) {}

TEST_F(function_test, using_as_callable) {
  const callable* callable = func.get();
  EXPECT_EQ("func", callable->identifier().string());
  auto params = callable->parameters();
  auto param1 = params.at(0);
  EXPECT_EQ(SINT_T, param1.type);
}
