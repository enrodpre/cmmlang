
#include "token.hpp"
#include <gtest/gtest.h>

#include <sys/types.h>
#include <utility>

using namespace cmm;

struct test_token : public ::testing::Test {
  u_int32_t counter = 0;
  location create_loc() {
    location l{counter, counter};
    counter++;
    return l;
  }

  template <typename... Args>
  tokens create_tokens(Args&&... args) {
    auto vec = std::array{std::forward<Args>(args)...} |
               std::views::transform([this](token_t t) { return token(t, create_loc()); }) |
               std::ranges::to<std::vector>();
    return tokens(vec);
  }
};
TEST_F(test_token, tokens) {
  token_t first  = token_t::and_;
  token_t second = token_t::assign;
  token_t third  = token_t::total;
  tokens t       = create_tokens(first, second, third);

  EXPECT_EQ(first, t.peek(0).type);
  EXPECT_EQ(first, t.next().type);
  EXPECT_EQ(second, t.peek(0).type);
  EXPECT_EQ(first, t.peek(-1).type);
  EXPECT_EQ(third, t.peek(1).type);
}
