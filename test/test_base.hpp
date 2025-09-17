#pragma once

#include "ast/expr.hpp"
#include <gtest/gtest.h>
#include <ostream>

using namespace cmm;
using namespace ast;

#define create_token(TYPE)          token(token_t::TYPE, cmm::location(1, 2));
#define create_token_val(TYPE, STR) token(token_t::TYPE, cmm::location(1, 2), STR);

template <std::formattable<char> T>
inline void PrintTo(const T& t, std::ostream* os) {
  *os << std::format("{}", t);
}
#define PRINT(x)                                                 \
  do {                                                           \
    testing::internal::CaptureStdout();                          \
    std::cout << x << '\n';                                      \
    std::string output = testing::internal::GetCapturedStdout(); \
    std::cout << output;                                         \
  } while (0);
