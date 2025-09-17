#include "ast/expr.hpp"
#include "parser.hpp"
#include "test_base.hpp"
#include <gtest/gtest.h>

namespace cmm {

void PrintTo(operator_t op, std::ostream* os) { *os << std::format("{}", op); }
}; // namespace cmm
struct ast_matcher {
  virtual ~ast_matcher()                            = default;
  virtual void match(const expr::expression&) const = 0;
};

struct indent_matcher : ast_matcher {
  std::string expected;
  explicit indent_matcher(std::string v)
      : expected(std::move(v)) {}

  void match(const expr::expression& e) const override {
    const auto* id = dynamic_cast<const expr::identifier*>(&e);
    EXPECT_TRUE(id);
    EXPECT_EQ(id->value(), expected);
  }
};

struct literal_matcher : ast_matcher {
  std::string expected;
  explicit literal_matcher(std::string v)
      : expected(std::move(v)) {}

  void match(const expr::expression& e) const override {
    const auto* lit = dynamic_cast<const expr::literal*>(&e);
    EXPECT_TRUE(lit);
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
    const auto* u = dynamic_cast<const expr::unary_operator*>(&e);
    EXPECT_TRUE(u);
    EXPECT_EQ(u->operator_.value(), op);
    sub->match(u->expr);
  }
};

struct binary_matcher : ast_matcher {
  operator_t op;
  std::unique_ptr<ast_matcher> lhs, rhs;

  binary_matcher(std::unique_ptr<ast_matcher> l, operator_t o, std::unique_ptr<ast_matcher> r)
      : op(o),
        lhs(std::move(l)),
        rhs(std::move(r)) {}

  void match(const expr::expression& e) const override {
    const expr::binary_operator* b = dynamic_cast<const expr::binary_operator*>(&e);
    EXPECT_TRUE(b);
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
    const auto* b = dynamic_cast<const expr::call*>(&e);
    EXPECT_TRUE(b);
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
  return std::make_unique<binary_matcher>(std::move(lhs), op, std::move(rhs));
}
inline std::unique_ptr<ast_matcher> Call(std::string ident,
                                         std::vector<std::unique_ptr<ast_matcher>> args) {
  return std::make_unique<call_matcher>(ident, std::move(args));
}

#define EXPECT_AST_EQ(expr, matcher) \
  do {                               \
    matcher->match(expr);            \
  } while (0)

class expression_test : public ::testing::Test {

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

TEST_F(expression_test, Call) {
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

TEST_F(expression_test, simple_binary) {
  parser::parser p({diez, plus, doce});
  EXPECT_AST_EQ(p.parse_expr(), Binary(Literal("10"), operator_t::plus, Literal("12")));
}

TEST_F(expression_test, compound_binary_no_paren) {
  parser::parser p({diez, plus, doce, star, ocho, plus, dos, star, quince});

  // clang-format off
  EXPECT_AST_EQ(p.parse_expr(),
                Binary(
                  Literal("10"),
                  operator_t::plus,
                  Binary(
                    Binary(
                      Literal("12"), 
                      operator_t::star, 
                      Literal("8")),
                    operator_t::plus,
                    Binary(
                      Literal("2"), 
                      operator_t::star,
                      Literal("15")))));
  // clang-format on
}

TEST_F(expression_test, pre_unary_operator) {
  parser::parser p({inc, ident});
  EXPECT_AST_EQ(p.parse_expr(), Unary(operator_t::pre_inc, Ident("var")));
}

TEST_F(expression_test, post_unary_operator) {
  parser::parser p({ident, dec});
  EXPECT_AST_EQ(p.parse_expr(), Unary(operator_t::post_dec, Ident("var")));
}

TEST_F(expression_test, simple_parenthesis) {
  parser::parser p({diez, plus, doce, star, x});
  EXPECT_AST_EQ(
      p.parse_expr(),
      Binary(Literal("10"), operator_t::plus, Binary(Literal("12"), operator_t::star, Ident("x"))));

  parser::parser p2({oparen, diez, plus, doce, cparen, star, x});
  EXPECT_AST_EQ(
      p2.parse_expr(),
      Binary(Binary(Literal("10"), operator_t::plus, Literal("12")), operator_t::star, Ident("x")));
}

TEST_F(expression_test, multi_operator) {
  parser::parser p({inc, exit, plus, doce, star, ident, dec});
  EXPECT_AST_EQ(
      p.parse_expr(),
      Binary(Unary(operator_t::pre_inc, Ident("exit")),
             operator_t::plus,
             Binary(Literal("12"), operator_t::star, Unary(operator_t::post_dec, Ident("var")))));
}

TEST_F(expression_test, operator_precedence) {
  parser::parser expr({x, assign, x, plus, ocho});
  // clang-format off
  EXPECT_AST_EQ(
      expr.parse_expr(),
      Binary(
        Ident("x"), 
        operator_t::assign,
        Binary(
          Ident("x"), 
          operator_t::plus,
          Literal("8")
      )));
  // clang-format on
}
