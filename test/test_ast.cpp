
#include "ast.hpp"
#include "expr.h"
#include "parser.hpp"
#include "test_base.hpp"
#include "token.hpp"
#include <gtest/gtest.h>

using namespace cmm;
using namespace parser;
using namespace memory;

struct AstTest : public ::testing::Test {
  tokens* every_token;
  // ~AstTest() override { delete every_token; }
  token constexpr_ = create_token(constexpr_);
  token static_    = create_token(static_);
  token o_curly    = create_token(o_curly);
  token c_curly    = create_token(c_curly);
  token exit       = create_token_val(ident, "exit");
  token doce       = create_token_val(int_lit, "12");
  token x          = create_token_val(ident, "x");
  token assign     = create_token(assign);
  token comma      = create_token(comma);
  token int_t      = create_token(int_t);
  token ident      = create_token_val(ident, "var");
  token lit        = create_token_val(int_lit, "5");
  token zero       = create_token_val(int_lit, "10");
  token plus       = create_token(plus);
  token ocho       = create_token_val(int_lit, "8");
  token dos        = create_token_val(int_lit, "2");
  token star       = create_token(star);
  token quince     = create_token_val(int_lit, "15");
  token inc        = create_token(inc);
  token y          = create_token_val(ident, "y");
  token dec        = create_token(dec);
  token semi       = create_token(semicolon);
  token if_        = create_token(if_);
  token oparen     = create_token(o_paren);
  token cparen     = create_token(c_paren);
  token true_      = create_token(true_lit);
  token else_      = create_token(else_);
};

TEST_F(AstTest, operator_precedence) {
  using namespace cmm::ast::expr;
  tokens expr{x, assign, x, plus, ocho, semi};
  cmm::parser::parser p(expr);

  expression& actual = p.parse_expr();
  auto* binop        = dynamic_cast<binary_operator*>(&actual);
  EXPECT_EQ(operator_t::assign, binop->operator_.value());
  EXPECT_EQ("x", dynamic_cast<expr::identifier*>(&binop->left)->value());
  auto* adding = dynamic_cast<expr::binary_operator*>(&binop->right);
  EXPECT_EQ("8", dynamic_cast<expr::literal*>(&adding->right)->value());
  EXPECT_EQ("x", dynamic_cast<expr::identifier*>(&adding->left)->value());
  EXPECT_EQ(operator_t::plus, adding->operator_.value());
}
