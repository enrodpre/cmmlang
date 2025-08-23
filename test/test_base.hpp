
#define PRINT(x)                                                 \
  do {                                                           \
    testing::internal::CaptureStdout();                          \
    std::cout << x << '\n';                                      \
    std::string output = testing::internal::GetCapturedStdout(); \
    std::cout << output;                                         \
  } while (0);
