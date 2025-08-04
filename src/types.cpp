#include "types.hpp"

#define TYPE_FORMAT_IMPL(TYPE, stdstr, ...) \
  std::string TYPE::name() const { \
    return std::format(stdstr, \
                       std::format("{}{}", is_const() ? "c" : "", is_volatile() ? "v" : ""), \
                       ##__VA_ARGS__); \
  }

namespace cmm {

std::string type::format() const {
  switch (category) {
    case type_category_t::void_t:
      return "void";
    case type_category_t::nullptr_t:
      return "nullptr_t";
    case type_category_t::bool_t:
      return std::format("{}{}bool", c ? "c" : "", v ? "v" : "");
    case type_category_t::char_t:
      return std::format("{}{}char", c ? "c" : "", v ? "v" : "");
    case type_category_t::uint_t:
      return std::format("{}{}uint", c ? "c" : "", v ? "v" : "");
    case type_category_t::sint_t:
      return std::format("{}{}sint", c ? "c" : "", v ? "v" : "");
    case type_category_t::float_t:
      return std::format("{}{}float", c ? "c" : "", v ? "v" : "");
    case type_category_t::lvalue_ref_t:
      return std::format("{}&{}{}", underlying->format(), c ? "c" : "", v ? "v" : "");
    case type_category_t::rvalue_ref_t:
      return std::format("{}&&{}{}", underlying->format(), c ? "c" : "", v ? "v" : "");
    case type_category_t::pointer_t:
      return std::format("{}*{}{}", underlying->format(), c ? "c" : "", v ? "v" : "");
    case type_category_t::array_t:
      return std::format("<{}, {}>", underlying->format(), rank);
    case type_category_t::scoped_enum_t:
    case type_category_t::unscoped_enum_t:
      return std::format("<{}, {}>", underlying->format(), rank);
    case type_category_t::class_t:
      return std::format("{}", underlying->format());
    case type_category_t::function_t:
    default:
      NOT_IMPLEMENTED;
  }
}
type_index::type_index(type_category_t t, size_t s)
    : type(t),
      value(s) {}

cr_type type::create_fundamental(type_category_t cat, bool b, bool v) { return create(cat, b, v); }

cr_type type::create_pointer(const type* t, bool b, bool v) {
  return create(type_category_t::pointer_t, t, b, v);
}
cr_type type::create_lvalue(const type* t, bool b, bool v) {
  return create(type_category_t::lvalue_ref_t, t, b, v);
}
namespace types {
  type_metadata_t get_metadata_of(type_category_t c) {
    return type_metadata_store.at(magic_enum::enum_index<type_category_t>(c).value());
  }
  // std::vector<type_category_t> all_children_of(type_category_t c) {
  //   const auto& [a, b, children] = get_metadata_of(c);
  //   ASSERT(a == c);
  //   return children |
  //          std::views::transform([](type_category_t child) { return all_children_of(child); }) |
  //          std::views::join | std::ranges::to<std::vector>();
  // }

  bool belongs_to(type_category_t child, type_category_t parent) {
    if (parent == child) {
      return true;
    }
    if (parent == type_category_t::any_t) {
      return true;
    }
    if (child == type_category_t::any_t) {
      return false;
    }
    const auto [self, parent_, children] = get_metadata_of(child);
    ASSERT(self == child);
    return belongs_to(parent_, parent);
  }
  bool belongs_to(cr_type t, type_category_t c) { return belongs_to(t.category, c); }
}; // namespace types
} // namespace cmm
