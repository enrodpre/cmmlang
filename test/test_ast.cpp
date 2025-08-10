
#include "ast.hpp"
#include "parser.hpp"
#include "token.hpp"
#include <gtest/gtest.h>

#include <utility>

using namespace cmm;
using namespace parser;
using namespace memory;

struct AstTest : public ::testing::Test {
  tokens* every_token;
  // ~AstTest() override { delete every_token; }
  static token create_token(cmm::token_t type, cmm::cstring value = "") {
    return {std::move(type), {}, value};
  }
  token class_     = create_token(token_t::class_t);
  token constexpr_ = create_token(token_t::constexpr_);
  token static_    = create_token(token_t::static_);
  token o_curly    = create_token(token_t::o_curly);
  token c_curly    = create_token(token_t::c_curly);
  token exit       = create_token(token_t::ident, "exit");
  token doce       = create_token(token_t::int_lit, "12");
  token x          = create_token(token_t::ident, "x");
  token assign     = create_token(token_t::assign);
  token comma      = create_token(token_t::comma);
  token int_t      = create_token(token_t::int_t);
  token ident      = create_token(token_t::ident, "var");
  token lit        = create_token(token_t::int_lit, "5");
  token zero       = create_token(token_t::int_lit, "10");
  token plus       = create_token(token_t::plus);
  token ocho       = create_token(token_t::int_lit, "8");
  token dos        = create_token(token_t::int_lit, "2");
  token star       = create_token(token_t::star);
  token quince     = create_token(token_t::int_lit, "15");
  token inc        = create_token(token_t::inc);
  token y          = create_token(token_t::ident, "y");
  token dec        = create_token(token_t::dec);
  token semi       = create_token(token_t::semicolon);
  token if_        = create_token(token_t::if_);
  token oparen     = create_token(token_t::o_paren);
  token cparen     = create_token(token_t::c_paren);
  token true_      = create_token(token_t::bool_lit, "true");
  token else_      = create_token(token_t::else_);

  template <typename T, typename U>
  T* trycast(U u) {
    EXPECT_NO_THROW(auto ptr = dynamic_cast<T*>(u));
    return dynamic_cast<T*>(u);
  }
};

TEST_F(AstTest, operator_precedence) {
  using namespace cmm::ast::expr;
  tokens expr{x, assign, x, plus, ocho, semi};
  cmm::parser::parser p(expr);

  expression* actual = p.parse_expr();
  auto* binop        = trycast<binary_operator>(actual);
  EXPECT_EQ(operator_t::assign, binop->operator_.type);
  EXPECT_EQ("x", trycast<identifier>(&binop->left)->term.value());
  auto* adding = trycast<binary_operator>(&binop->right);
  EXPECT_EQ("1", trycast<literal>(&adding->right)->term.value());
  EXPECT_EQ("x", trycast<identifier>(&adding->left)->term.value());
  EXPECT_EQ(_operator_t::plus, adding->operator_.type.inner());
}
