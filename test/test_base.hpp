

#define EXPECT_NOTHROW_AND_RETURN(CAT, ARG, PARAM)                    \
  EXPECT_NO_THROW(bind_argument(value_category_t::CAT, ARG, PARAM));  \
  EXPECT_EQ(PARAM, bind_argument(value_category_t::CAT, ARG, PARAM));

#define PRINT(x)                                                 \
  do {                                                           \
    testing::internal::CaptureStdout();                          \
    std::cout << x << '\n';                                      \
    std::string output = testing::internal::GetCapturedStdout(); \
    std::cout << output;                                         \
  } while (0);
