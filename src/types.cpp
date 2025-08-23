#include "types.hpp"
#include "common.hpp"

#include <format>

namespace cmm {

std::string type::string() const {
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
      return std::format("{}&{}{}", underlying->string(), c ? "c" : "", v ? "v" : "");
    case type_category_t::rvalue_ref_t:
      return std::format("{}&&{}{}", underlying->string(), c ? "c" : "", v ? "v" : "");
    case type_category_t::pointer_t:
      return std::format("{}*{}{}", underlying->string(), c ? "c" : "", v ? "v" : "");
    case type_category_t::array_t:
      return std::format("<{}, {}>", underlying->string(), rank);
    case type_category_t::scoped_enum_t:
    case type_category_t::unscoped_enum_t:
      return std::format("<{}, {}>", underlying->string(), rank);
    case type_category_t::class_t:
      return std::format("{}", underlying->string());
    case type_category_t::function_t:
    default:
      return "";
  }
}

[[nodiscard]] ptype type::create_void() { return create(type_category_t::void_t); }

[[nodiscard]] ptype type::create_fundamental(type_category_t cat, bool b, bool v) {
  return create(cat, nullptr, 0, b, v);
}

[[nodiscard]] ptype type::create_pointer(ptype t, bool b, bool v) {
  return create(type_category_t::pointer_t, t, 0, b, v);
}

[[nodiscard]] ptype type::create_lvalue(ptype t, bool b, bool v) {
  return create(type_category_t::lvalue_ref_t, t, 0, b, v);
}
[[nodiscard]] ptype type::create_rvalue(ptype t, bool b, bool v) {
  return create(type_category_t::rvalue_ref_t, t, 0, b, v);
}

[[nodiscard]] ptype type::create_array(ptype t, size_t n, bool b, bool v) {
  return create(type_category_t::array_t, t, n, b, v);
}

[[nodiscard]] ptype type::create_string(size_t n, bool b, bool v) {
  auto under = create_fundamental(type_category_t::char_t, b, v);
  return create_array(under, n, b, v);
}

bool belongs_to(const type_category_data& child, type_category_t parent) {
  if (parent == child.self) {
    return true;
  }
  if (child.self == type_category_t::generic_t || child.self == type_category_t::dummy_t) {
    return false;
  }
  if (parent == type_category_t::any_t) {
    return true;
  }
  if (child.self == type_category_t::any_t) {
    return false;
  }
  return belongs_to(child.parent, parent);
}

[[nodiscard]] bool type_matcher::match(crptype_spec spec) const {
  if (const auto& type_ = std::dynamic_pointer_cast<const type>(spec)) {
    return m_matcher(type_);
  }
  return typeid(this) == typeid(spec);
}

bool type_matcher::operator()(crptype lhs) const { return m_matcher(lhs); }

std::string type_matcher::string() const { return m_desc; }

type_matcher type_matcher::operator&&(const type_matcher& m) const {
  return {std::format("{} and {}", *this, m),
          [*this, &m](crptype t) { return this->m_matcher(t) && m.m_matcher(t); }};
}

type_matcher type_matcher::operator||(const type_matcher& m) const {
  return {std::format("{} or {}", *this, m),
          [*this, &m](crptype t) { return this->m_matcher(t) || m.m_matcher(t); }};
}

type_matcher type_matcher::operator!() const {
  return {std::format("not {}", *this),
          [this](crptype t) { return !this->m_matcher(t); }}; // namespace cmm
}

ptype type_modifier::operator()(crptype t) const { return m_modifier(t); }

type_modifier type_modifier::operator|(const type_modifier& other) const {
  return {std::format("{}, {}", m_desc, other.m_desc),
          [&other, *this](crptype t) -> ptype { return other(m_modifier(t)); }};
}

bool type_converter::is_convertible(crptype t) const { return m_from->match(t); }

ptype type_converter::operator()(ptype t) const {
  if (!is_convertible(t)) {
    throw error(std::format("Type {} is not convertible with {}", t, *this));
  }
  return m_converter(t);
}

using namespace matchers;

// const type_converter nullptr_to_ptr =
//     cmm::type_converter::builder().name("Nullptr to
//     pointer").from(NULLPTR_T).with() >> &is_pointer;
// const type_converter bool_to_any      = cmm::type_converter::builder().name("Bool to

std::vector<ptype> get_convertible_types(crptype from) {
  return conversions::standard | std::views::filter([from](const type_converter* conv) {
           return conv->is_convertible(from);
         }) |
         std::views::transform(
             [&from](const type_converter* converter) { return (*converter)(from); }) |
         std::ranges::to<std::vector>();
}

bool is_convertible(crptype from, crptype to) {
  if (from == to) {
    return true;
  }
  auto converted =
      get_convertible_types(from) | std::views::filter([to](crptype t) { return t == to; });
  return !std::ranges::empty(converted);
}
} // namespace cmm
