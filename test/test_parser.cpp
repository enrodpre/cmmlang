
#include "ast.hpp"
#include "expr.h"
#include "parser.hpp"
#include "test_base.hpp"
#include "types.hpp"
#include <gtest/gtest.h>

using namespace cmm;
using namespace ast;

struct ast_matcher {
  virtual ~ast_matcher()                            = default;
  virtual void match(const expr::expression&) const = 0;
};

struct indent_matcher : ast_matcher {
  std::string expected;
  explicit indent_matcher(std::string v)
      : expected(std::move(v)) {}

  void match(const expr::expression& e) const override {
    auto* id = dynamic_cast<const identifier*>(&e);
    ASSERT_NE(id, nullptr);
    EXPECT_EQ(id->value(), expected);
  }
};

struct literal_matcher : ast_matcher {
  std::string expected;
  explicit literal_matcher(std::string v)
      : expected(std::move(v)) {}

  void match(const expr::expression& e) const override {
    auto* lit = dynamic_cast<const literal*>(&e);
    ASSERT_NE(lit, nullptr);
    EXPECT_EQ(lit->value(), expected);
  }
};

struct unary_matcher : ast_matcher {
  operator_t op;
  std::unique_ptr<ast_matcher> sub;

  unary_matcher(operator_t o, std::unique_ptr<ast_matcher> s)
      : op(o),
        sub(std::move(s)) {}

  void match(const expr::expression& e) const override {
    auto* u = dynamic_cast<const expr::unary_operator*>(&e);
    ASSERT_NE(u, nullptr);
    EXPECT_EQ(u->operator_.value(), op);
    sub->match(u->expr);
  }
};

struct binary_matcher : ast_matcher {
  operator_t op;
  std::unique_ptr<ast_matcher> lhs, rhs;

  binary_matcher(operator_t o, std::unique_ptr<ast_matcher> l, std::unique_ptr<ast_matcher> r)
      : op(o),
        lhs(std::move(l)),
        rhs(std::move(r)) {}

  void match(const expr::expression& e) const override {
    auto* b = dynamic_cast<const expr::binary_operator*>(&e);
    ASSERT_NE(b, nullptr);
    EXPECT_EQ(b->operator_.value(), op);
    lhs->match(b->left);
    rhs->match(b->right);
  }
};

struct call_matcher : ast_matcher {
  std::string identifier;
  std::vector<std::unique_ptr<ast_matcher>> args;

  call_matcher(std::string ident, std::vector<std::unique_ptr<ast_matcher>>&& t_args)
      : identifier(ident),
        args(std::move(t_args)) {}

  void match(const expr::expression& e) const override {
    auto* b = dynamic_cast<const expr::call*>(&e);
    ASSERT_NE(b, nullptr);
    EXPECT_EQ(b->ident.value(), identifier);
    for (const auto& [actual, matcher] : std::views::zip(b->args, args)) {
      matcher->match(*actual);
    }
  }
};

inline std::unique_ptr<ast_matcher> Ident(std::string v) {
  return std::make_unique<indent_matcher>(std::move(v));
}
inline std::unique_ptr<ast_matcher> Literal(std::string v) {
  return std::make_unique<literal_matcher>(std::move(v));
}
inline std::unique_ptr<ast_matcher> Unary(operator_t op, std::unique_ptr<ast_matcher> sub) {
  return std::make_unique<unary_matcher>(op, std::move(sub));
}
inline std::unique_ptr<ast_matcher> Binary(std::unique_ptr<ast_matcher> lhs,
                                           operator_t op,
                                           std::unique_ptr<ast_matcher> rhs) {
  return std::make_unique<binary_matcher>(op, std::move(lhs), std::move(rhs));
}
inline std::unique_ptr<ast_matcher> Call(std::string ident,
                                         std::vector<std::unique_ptr<ast_matcher>> args) {
  return std::make_unique<call_matcher>(ident, std::move(args));
}

#define EXPECT_AST_EQ(expr, matcher) \
  do {                               \
    matcher->match(expr);            \
  } while (0)

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

TEST_F(ParserTest, Call) {
  parser::parser p({exit, oparen, diez, comma, doce, cparen, semi});

  auto* call_ = cast<expr::call*>(&p.parse_expr());
  EXPECT_TRUE(call_);
  EXPECT_EQ("exit", call_->ident.value());
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

  EXPECT_EQ(vardecl->ident.value(), "var");
  auto* expr = cast<expr::literal*>(vardecl->init);
  EXPECT_EQ("5", expr->value());
  EXPECT_EQ(cmm::types::core_t::sint_t, expr->type()->categorize());
}

TEST_F(ParserTest, binop_expression) {
  parser::parser p({diez, plus, doce, star, ocho, plus, dos, star, quince});
  auto& top_expr = p.parse_expr();

  EXPECT_AST_EQ(top_expr,
                Binary(Binary(Literal("10"),
                              operator_t::star,
                              Binary(Literal("12"), operator_t::star, Literal("8"))),
                       operator_t::plus,
                       Binary(Literal("2"), operator_t::star, Literal("15"))));
}

TEST_F(ParserTest, unary_operator) {
  auto& expr = parser::parser({inc, ident}).parse_expr();
  EXPECT_AST_EQ(expr, Unary(operator_t::pre_inc, Literal("var")));

  expr = parser::parser({ident, dec}).parse_expr();
  EXPECT_AST_EQ(expr, Unary(operator_t::post_dec, Literal("var")));
}

TEST_F(ParserTest, multi_operator) {
  auto& expr = parser::parser({inc, exit, plus, doce, star, ident, dec}).parse_expr();
  EXPECT_AST_EQ(
      expr,
      Binary(Unary(operator_t::pre_dec, Literal("x")),
             operator_t::plus,
             Binary(Literal("12"), operator_t::star, Unary(operator_t::post_inc, Literal("y")))));
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

TEST_F(ParserTest, operator_precedence) {
  parser::parser expr({x, assign, x, plus, ocho, semi});
  EXPECT_AST_EQ(expr.parse_expr(),
                Binary(Literal("x"),
                       operator_t::assign,
                       Binary(Literal("x"), operator_t::plus, Literal("8"))));
}
