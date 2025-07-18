#include "strings.hpp"
#include "gtest/gtest.h"

using namespace cmm::strings;

TEST(dynamic_buffer, dump)
{
  string_buffer text;
  text.create();
  text.write("before");

  text.create();
  text.write("after");
  auto dumped = text.dump();
  EXPECT_EQ(dumped, "after");

  auto dumped2 = text.dump();
  EXPECT_EQ(dumped2, "before");
}

TEST(dynamic_buffer, save_and_load)
{
  string_buffer text;

  text.create();
  EXPECT_NO_THROW(text.write("1"));

  text.create();
  EXPECT_NO_THROW(text.write("3"));
  auto f = text.snapshot();

  text.save();
  EXPECT_NO_THROW(text.write("2"));
  auto f2 = text.snapshot();
  EXPECT_NE(f, f2);

  text.load();
  text.write("\n");
  auto f3 = text.snapshot();
  EXPECT_EQ("123\n", f3);
}
