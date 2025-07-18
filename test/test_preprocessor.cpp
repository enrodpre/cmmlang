#include "preprocessor.hpp"
#include <gtest/gtest.h>

using namespace cmm;
TEST(TestPreprocessor, SingleLineComments)
{
  std::string expected = "code prueba\nx";

  Preprocessor preproccessor =
      Preprocessor("code prueba//hola\nx//to be removed");
  preproccessor.remove_single_line_comments();
  auto actual = preproccessor.string();

  EXPECT_EQ(expected, actual);
}

TEST(TestPreprocessor, MultiBlockComments)
{
  std::string expected = "code x\n";

  Preprocessor preproccessor =
      Preprocessor("code /*to be removed*/x\n/* this aswell */");
  preproccessor.remove_block_comments();
  auto actual = preproccessor.string();

  EXPECT_EQ(expected, actual);
}
