
#include "ast.hpp"
#include "lang.hpp"
#include "parser.hpp"
#include "types.hpp"
#include <cpptrace/from_current.hpp>
#include <gtest/gtest.h>
#include <utility>

using namespace cmm;
using namespace ast;

class ParserTest : public ::testing::Test {
  static cmm::location create_location() {
    static size_t i = 0;
    --i;
    return {i, i, i, i};
  }
  static token create_token(cmm::token_t type, cmm::cstring value = "") {
    location loc = create_location();
    return {std::move(type), location(loc.rows.start, 1, loc.cols.start, loc.cols.length), value};
  }

protected:
  // token inline_    = create_token(token_t::inline_);
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

  size_t row;

  template <typename To, typename From>
  To cast(From from) {
    auto* to = dynamic_cast<To>(from);
    EXPECT_TRUE(to != nullptr) << std::format("Type {} assumed to be {} but is {}",
                                              typeid(From).name(),
                                              typeid(To).name(),
                                              typeid(from).name());
    return to;
  }

#define CAST(FROM, TO_T, TO) \
  auto* TO = dynamic_cast<TO_T>(FROM); \
  EXPECT_TRUE(TO != nullptr) << std::format( \
      "Type {} assumed to be {}", typeid(FROM).name(), typeid(TO_T).name());

  static void check_literal(const expr::expression& expr, const std::string& value) {
    const auto* lit = dynamic_cast<const expr::literal*>(&expr);
    EXPECT_FALSE(lit == nullptr);
    EXPECT_EQ(value, lit->term.value);
  }
  static void require_identifier(const expr::expression& expr, const std::string& value) {
    const auto* lit = dynamic_cast<const expr::identifier*>(&expr);
    EXPECT_FALSE(lit == nullptr);
    EXPECT_EQ(value, lit->term.value);
  }

  template <typename T>
  static T* unfold_expression(expr::expression* expr) {
    auto res = dynamic_cast<T*>(expr);
    EXPECT_TRUE(res != nullptr);
    return res;
  }
};

using std::string;

using namespace std;
using namespace expr;

TEST_F(ParserTest, Call) {
  parser::parser p({exit, oparen, zero, comma, doce, cparen, semi});

  auto* call_ = cast<expr::call*>(p.parse_expr());
  EXPECT_TRUE(call_);
  EXPECT_EQ("exit", call_->ident.value);
  EXPECT_EQ(2, call_->args.size());

  auto it                = (*call_).args.begin();
  expr::expression* arg1 = *it++;
  check_literal(*arg1, "10");
  expr::expression* arg2 = *it++;
  check_literal(*arg2, "12");
}

TEST_F(ParserTest, Vardecl) {
  parser::parser p({int_t, ident, assign, lit, semi});
  auto* elements = p.parse_declaration();
  auto* vardecl  = cast<decl::variable*>(elements);

  EXPECT_EQ(vardecl->ident->value, "var");
  auto* expr = cast<expr::literal*>(vardecl->init);
  EXPECT_EQ("5", expr->term.value);
  EXPECT_EQ(type_category_t::sint_t, expr->type);
}

#define PRINT(x) \
  do { \
    testing::internal::CaptureStdout(); \
    std::cout << x << '\n'; \
    std::string output = testing::internal::GetCapturedStdout(); \
    std::cout << output; \
  } while (0);

TEST_F(ParserTest, binop_expression) {
  // TODO terminar

  parser::parser p({zero, plus, doce, star, ocho, plus, dos, star, quince});
  auto* top_expr  = p.parse_expr();

  auto* top_binop = unfold_expression<expr::binary_operator>(top_expr);
  check_literal(top_binop->left, "0");

  auto* top_right = unfold_expression<expr::binary_operator>(&top_binop->right);

  // Check left
  auto* mult_left = unfold_expression<expr::binary_operator>(&top_right->left);
  check_literal(mult_left->left, "12");
  check_literal(mult_left->right, "8");

  // Check right
  auto* mult_right = unfold_expression<expr::binary_operator>(&top_right->right);
  check_literal(mult_right->left, "2");
  check_literal(mult_right->right, "15");
}

TEST_F(ParserTest, unary_operator) {
  auto* expr   = parser::parser({inc, ident}).parse_expr();
  auto* preinc = cast<expr::unary_operator*>(expr);
  EXPECT_EQ(operator_t::pre_inc, preinc->operator_.type);

  expr          = parser::parser({ident, dec}).parse_expr();
  auto* postdec = cast<expr::unary_operator*>(expr);
  EXPECT_EQ(operator_t::post_dec, preinc->operator_.type);
}

TEST_F(ParserTest, multi_operator) {
  auto* expr = parser::parser({inc, exit, plus, doce, star, ident, dec}).parse_expr();
  CAST(expr, expr::binary_operator*, first);
  EXPECT_EQ(operator_t::plus, first->operator_.type);

  CAST(&first->left, expr::unary_operator*, first_left);
  EXPECT_EQ(operator_t::pre_inc, first_left->operator_.type);
  CAST(&first_left->expr, expr::identifier*, left_id);
  EXPECT_EQ("exit", left_id->term.value);

  CAST(&first->right, expr::binary_operator*, first_right);
  EXPECT_EQ(operator_t::star, first_right->operator_.type);
  CAST(&first_right->left, expr::identifier*, right_left);
  EXPECT_EQ("12", right_left->term.value);
  CAST(&first_right->right, expr::unary_operator*, right_right);
  EXPECT_EQ(operator_t::post_dec, right_right->operator_.type);
  CAST(&right_right->expr, expr::identifier*, right_right_right);
  EXPECT_EQ("var", right_right_right->term.value);
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
  auto* block = cast<compound*>(if_->block);
  auto* else_ = cast<compound*>(if_->else_);

  EXPECT_EQ(block->size(), 0);
  EXPECT_TRUE(if_->else_);
  EXPECT_EQ(else_->size(), 0);
}

TEST_F(ParserTest, Block) {
  parser::parser p({o_curly, o_curly, semi, c_curly, semi, o_curly, c_curly, c_curly});

  auto* block = p.parse_compound();
  EXPECT_EQ(block->size(), 2);

  auto* first_stmt = *block->begin();
  auto* comp       = cast<compound*>(first_stmt);
  EXPECT_EQ(0, comp->size());
}

#if 0
TEST_F(ParserTest, specifiers)
{
  std::vector<token> tokens_{
      int_t,
      static_,
      inline_,
      constexpr_,
      x,
  };
  tokens tokens{tokens_};
  Parser parser{tokens};

  auto specs = parser.parse_specifiers();

  EXPECT_TRUE(specs.is_inline);
  EXPECT_TRUE(specs.is_constexpr);
  EXPECT_EQ(specs.type, Specifier::int_t);
  EXPECT_EQ(specs.storage, Specifier::static_);

  std::vector<token> tokens_2{
      int_t,
      x,
      static_,
      inline_,
      constexpr_,
  };

  tokens tokens2{tokens_2};
  Parser parser2{tokens2};

  auto specs2 = parser2.parse_specifiers();

  EXPECT_FALSE(specs2.is_inline);
  EXPECT_FALSE(specs2.is_constexpr);
  EXPECT_EQ(specs2.type, Specifier::int_t);
  EXPECT_EQ(specs2.storage, Specifier::no_specifier);

  std::vector<token> tokens_3{
      x,
      int_t,
      static_,
      inline_,
      constexpr_,
  };
  tokens tokens3{tokens_3};
  Parser parser3{tokens3};

  EXPECT_THROW(parser3.parse_specifiers(), SpecifierParserException);

  std::vector<token> tokens_4({static_, inline_, constexpr_, zero});

  tokens tokens4{tokens_4};
  Parser parser4{tokens4};

  EXPECT_THROW(parser4.parse_specifiers(), SpecifierParserException);
}
#endif
