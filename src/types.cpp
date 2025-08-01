#include "types.hpp"

#define TYPE_FORMAT_IMPL(TYPE, stdstr, ...) \
  std::string TYPE::name() const { \
    return std::format(stdstr, \
                       std::format("{}{}", is_const() ? "c" : "", is_volatile() ? "v" : ""), \
                       ##__VA_ARGS__); \
  }

namespace cmm {

type_index::type_index(type_t t, size_t s)
    : type(std::move(t)),
      value(s) {}

} // namespace cmm
