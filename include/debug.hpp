#pragma once

#include <cxxabi.h>



template <typename T>
inline std::string type_name()
{
  int status = 0;
  char* demangled =
      abi::__cxa_demangle(typeid(T).name(), nullptr, nullptr, &status);
  std::string result(demangled);
  std::free(demangled);
  return result;
}
