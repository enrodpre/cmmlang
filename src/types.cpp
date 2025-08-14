#include "types.hpp"

#include <algorithm>

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

cr_type type::create_array(const type* t, size_t n, bool b, bool v) {
  return create(type_category_t::array_t, t, n, b, v);
}

cr_type type::create_string(size_t n, bool b, bool v) {
  const auto& under = create_fundamental(type_category_t::char_t, b, v);
  return create_array(&under, n, b, v);
}

result<ptr_type> is_assignable(cr_type from, cr_type to) {
  if (from == to) {
    return {&to};
  }
  auto conver = conversions::get_convertibles(&from);
  return {*std::ranges::find_if(conver, [&to](ptr_type t) { return (*t) == to; })};
}

type_metadata_t get_metadata_of(type_category_t c) {
  return type_metadata_store.at(magic_enum::enum_index<type_category_t>(c).value());
}

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
} // namespace cmm
