
#include "ast/tree.hpp"
#include "ast/expr.hpp"
#include "parser.hpp"
#include "test_base.hpp"
#include "types.hpp"
#include <gtest/gtest.h>

using namespace cmm;
using namespace ast;

class ParserTest : public ::testing::Test {

protected:
  token class_     = create_token(class_t);
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
  token diez       = create_token_val(int_lit, "10");
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

  template <typename To, typename From>
  To cast(From from) {
    auto* to = dynamic_cast<To>(from);
    EXPECT_TRUE(to != nullptr) << std::format("Type {} assumed to be {} but is {}",
                                              typeid(From).name(),
                                              typeid(To).name(),
                                              typeid(from).name());
    return to;
  }

  static void check_literal(const expr::expression& expr, const std::string& value) {
    const auto* lit = dynamic_cast<const expr::literal*>(&expr);
    EXPECT_FALSE(lit == nullptr);
    EXPECT_EQ(value, lit->value());
  }
};

using std::string;

using namespace std;
using namespace expr;

TEST_F(ParserTest, token_data) {
  auto t = token_t::plus;
  EXPECT_TRUE(token_data(t).is_binary_operator());
  EXPECT_TRUE(token_t::binary_begin < t && t < token_t::binary_end);
  EXPECT_TRUE(token_data(t).is_unary_operator());
}

TEST_F(ParserTest, IfElse) {
  parser::parser p({if_,
                    oparen,
                    true_,
                    cparen,
                    o_curly,
                    semi,
                    semi,
                    semi,
                    semi,
                    c_curly,
                    else_,
                    o_curly,
                    semi,
                    c_curly});

  auto* elems = p.parse_if();
  auto* if_   = cast<selection::if_*>(elems);
  auto* block = cast<ast::decl::block*>(if_->block);
  auto* else_ = cast<ast::decl::block*>(if_->else_);

  EXPECT_EQ(block->stmts.size(), 0);
  EXPECT_TRUE(if_->else_);
  EXPECT_EQ(else_->stmts.size(), 0);
}

TEST_F(ParserTest, Block) {
  parser::parser p({o_curly, o_curly, semi, c_curly, semi, o_curly, c_curly, c_curly});

  auto* block = p.parse_block();
  EXPECT_EQ(block->stmts.size(), 2);

  auto* first_stmt = *block->stmts.begin();
  auto* comp       = cast<decl::block*>(first_stmt);
  EXPECT_EQ(0, comp->stmts.size());
}

TEST_F(ParserTest, Vardecl) {
  parser::parser p({int_t, ident, assign, lit, semi});
  auto* elements = p.parse_declaration();
  auto* vardecl  = cast<decl::variable*>(elements);

  EXPECT_EQ(vardecl->ident.value(), "var");
  auto* expr = cast<expr::literal*>(vardecl->init);
  EXPECT_EQ("5", expr->value());
  EXPECT_EQ(cmm::types::core_t::sint_t, expr->type()->categorize());
}
