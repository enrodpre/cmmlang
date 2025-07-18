
#include "allocator.hpp"
#include "ast.hpp"
#include "parser.hpp"
#include <cpptrace/from_current.hpp>
#include <gtest/gtest.h>
#include <memory>
#include <utility>

using namespace cmm;
using namespace ast;
using namespace parser;

class ParserTest : public ::testing::Test {
  cmm::location create_location() {
    static size_t i = 0;
    --i;
    return {i, i, i, i};
  }
  token create_token(cmm::token_t type, cmm::cstring value = "") {
    location loc = create_location();
    return {std::move(type),
            location(loc.rows.start, 1, loc.cols.start, loc.cols.length),
            value};
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

  static std::unique_ptr<Parser> prepare_parser(std::vector<token> t) {
    return std::make_unique<Parser>(tokens(t));
  }

  template <typename T>
  T unfold(compound compound) {
    return get<T>(*compound.front());
  }

  template <typename T>
  T unfold(statement* stmt) {
    return get<T>(*stmt);
  }

  static void check_literal(const expr::expression& expr,
                            const std::string& value) {
    const auto* lit = dynamic_cast<const expr::literal*>(&expr);
    EXPECT_FALSE(lit == nullptr);
    EXPECT_EQ(value, lit->lit.value);
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
  auto parser = prepare_parser({exit, oparen, zero, comma, doce, cparen, semi});
  auto elements = parser->parse();

  EXPECT_EQ(elements.size(), 1);
  auto* expr_call = get_if<expr::expression*>(elements[0]);
  EXPECT_TRUE(expr_call);
  auto* call_ = dynamic_cast<expr::call*>(*expr_call);
  EXPECT_TRUE(call_);
  EXPECT_EQ((*call_).ident->value, "exit");
  EXPECT_EQ((*call_).args.size(), 2);

  auto it                = (*call_).args.begin();
  expr::expression* arg1 = *it++;
  check_literal(*arg1, "10");
  expr::expression* arg2 = *it++;
  check_literal(*arg2, "12");
}

TEST_F(ParserTest, Vardeclaration) {
  auto parser   = prepare_parser({int_t, ident, assign, lit, semi});
  auto elements = parser->parse();
  auto vardecl  = unfold<declaration::variable>(elements);

  // EXPECT_EQ(vardecl->specs.type, cmm::Specifier::int_t);
  EXPECT_EQ(vardecl.ident->value, "var");
  EXPECT_TRUE(vardecl.init);
}

TEST_F(ParserTest, expression) {
  // TODO terminar

  auto parser =
      prepare_parser({zero, plus, doce, star, ocho, plus, dos, star, quince});
  auto* top_expr  = parser->parse_expr();

  auto* top_binop = unfold_expression<expr::binary_operator>(top_expr);
  check_literal(top_binop->left, "0");

  auto* top_right = unfold_expression<expr::binary_operator>(&top_binop->right);

  // Check left
  auto* mult_left = unfold_expression<expr::binary_operator>(&top_right->left);
  check_literal(mult_left->left, "12");
  check_literal(mult_left->right, "8");

  // Check right
  auto* mult_right =
      unfold_expression<expr::binary_operator>(&top_right->right);
  check_literal(mult_right->left, "2");
  check_literal(mult_right->right, "15");
}

TEST_F(ParserTest, unary_operator) {
  auto parser = prepare_parser({inc, ident, semi, ident, dec, semi});
  auto elems  = parser->parse();

  EXPECT_EQ(2, elems.size());
  auto it     = elems.begin();
  auto* expr1 = get_if<expr::expression*>(*it++);
  EXPECT_TRUE(expr1);
  auto* preinc = unfold_expression<expr::unary_operator>(*expr1);
  EXPECT_TRUE(preinc);
  auto* expr2 = get_if<expr::expression*>(*it++);
  EXPECT_TRUE(expr2);
  auto* postdec = unfold_expression<expr::unary_operator>(*expr2);
  EXPECT_TRUE(postdec);

  EXPECT_EQ(cmm::operator_t::pre_inc, (*preinc).operator_.type);
  EXPECT_EQ(cmm::operator_t::post_dec, (*postdec).operator_.type);
}

TEST_F(ParserTest, IfElse) {

  auto parser = prepare_parser({if_,
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

  auto elems  = parser->parse();
  auto if_    = unfold<selection::if_>(elems);
  auto block  = unfold<compound>(&if_.block);
  auto else_  = unfold<compound>(if_.else_);

  EXPECT_EQ(block.size(), 0);
  EXPECT_TRUE(if_.else_);
  EXPECT_EQ(else_.size(), 0);
}

TEST_F(ParserTest, Block) {
  auto parser = prepare_parser(
      {o_curly, o_curly, semi, c_curly, semi, o_curly, c_curly, c_curly});

  auto block = parser->parse_compound<true>();
  EXPECT_EQ(block.size(), 2);

  auto* first_stmt = block.front();
  EXPECT_NO_THROW(std::get<compound>(*first_stmt));
  compound comp = std::get<compound>(*first_stmt);
  EXPECT_EQ(0, comp.size());
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
