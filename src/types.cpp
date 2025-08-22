#include "types.hpp"

#include <format>
#include <utility>
#include <utils.hpp>

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
      NOT_IMPLEMENTED;
  }
}

crtype type::create_void() { return create(type_category_t::void_t); }
crtype type::create_fundamental(type_category_t cat, bool b, bool v) { return create(cat, b, v); }

crtype type::create_pointer(const type* t, bool b, bool v) {
  return create(type_category_t::pointer_t, t, b, v);
}
crtype type::create_lvalue(const type* t, bool b, bool v) {
  return create(type_category_t::lvalue_ref_t, t, b, v);
}

crtype type::create_array(const type* t, size_t n, bool b, bool v) {
  return create(type_category_t::array_t, t, n, b, v);
}

crtype type::create_string(size_t n, bool b, bool v) {
  const auto& under = create_fundamental(type_category_t::char_t, b, v);
  return create_array(&under, n, b, v);
}

bool belongs_to(const type_category_data& child, type_category_t parent) {
  if (parent == child.self) {
    return true;
  }
  if (child.self == type_category_t::matcher_t || child.self == type_category_t::dummy_t) {
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

type_matcher::type_matcher(value_type&& v)
    : type_specifier(type_category_t::matcher_t),
      m_matcher(std::move(v)),
      bound_type(nullptr) {}
type_matcher::type_matcher(const value_type& v)
    : type_specifier(type_category_t::matcher_t),
      m_matcher(v),
      bound_type(nullptr) {}
type_matcher::operator value_type() const { return m_matcher; }

bool type_matcher::match(const type_specifier& other) const {
  // matcher vs matcher
  if (typeid(*this) == typeid(other)) {
    return true;
  }
  // matcher vs type
  if (const auto* t = dynamic_cast<const type*>(&other)) {
    bool result = std::invoke(m_matcher, t);
    if (result) {
      bound_type = t;
    }
    return result;
  }
  return false;
}

std::string type_matcher::string() const { return cpptrace::demangle(typeid(this).name()); }

type_matcher type_matcher::operator&&(const type_matcher& m) const {
  return {[this, m](ptype t) { return m_matcher(t) && m.m_matcher(t); }};
}
type_matcher type_matcher::operator||(const type_matcher& m) const {
  return {[this, m](ptype t) { return m_matcher(t) || m.m_matcher(t); }};
}
type_matcher type_matcher::operator!() const {
  return {[this](ptype t) { return !m_matcher(t); }};
}

namespace {
constexpr type_matcher belongs_to(type_category_t cat) {
  return {[cat](ptype t) { return belongs_to(t->category, cat); }};
}
constexpr type_matcher is_category(type_category_t cat) {
  return {[cat](ptype t) { return t->category == cat; }};
}
constexpr auto new_type(type_category_t cat) {
  return [cat](ptype) -> ptype { return &type::create(cat); };
}

} // namespace
namespace matchers {
const type_matcher is_integral   = belongs_to(type_category_t::integral_t);
const type_matcher is_arithmetic = belongs_to(type_category_t::arithmetic_t);
const type_matcher is_ref        = is_category(type_category_t::lvalue_ref_t);
const type_matcher is_pointer    = is_category(type_category_t::pointer_t);
const type_matcher is_floating   = is_category(type_category_t::float_t);
const type_matcher is_unscoped   = is_category(type_category_t::unscoped_enum_t);
const type_matcher is_array      = is_category(type_category_t::array_t);
const type_matcher is_const      = type_matcher([](ptype t) { return t->c; });
const type_matcher is_cref       = is_ref && is_const;
const type_matcher is_volatile   = type_matcher([](ptype t) { return t->v; });
const type_matcher any           = type_matcher([](ptype) { return true; });
} // namespace matchers

type_converter::builder& type_converter::builder::name(std::string desc) {
  m_obj.m_desc = std::move(desc);
  return *this;
}
type_converter::builder& type_converter::builder::from(const type_matcher& matcher) {
  m_obj.m_from = std::make_unique<type_matcher>(matcher);
  return *this;
}
type_converter::builder& type_converter::builder::from(ptype t) {
  m_obj.m_from = std::make_unique<type>(*t);
  return *this;
}
type_converter::builder& type_converter::builder::to(ptype t) {
  m_obj.m_converter = [&t](ptype) { return t; };
  return *this;
}

type_converter::builder& type_converter::builder::with(type_converter_t fn) {
  m_obj.m_converter = std::move(fn);
  return *this;
}
type_converter type_converter::builder::build() { return std::move(m_obj); }

bool type_converter::is_convertible(ptype t) const { return m_from->match(*t); }

ptype type_converter::operator()(ptype t) const {
  if (!is_convertible(t)) {
    throw error(std::format("Type {} is not convertible with {}", t, *this));
  }
  return m_converter(t);
}

namespace conversions {
namespace {
constexpr auto get_underlying = [](ptype t) -> ptype { return t->underlying; };

constexpr auto get_pointer    = [](ptype t) -> ptype {
  return &type::create_pointer(t->underlying, t->c, t->v);
};

} // namespace
using namespace matchers;

// const type_converter nullptr_to_ptr =
//     cmm::type_converter::builder().name("Nullptr to pointer").from(NULLPTR_T).with() >>
//     &is_pointer;
const type_converter identity = type_converter::builder()
                                    .name("Type converter identity")
                                    .from(any)
                                    .with([](ptype t) { return t; })
                                    .build();
const type_converter any_to_bool =
    type_converter::builder()
        .name("Any to bool")
        .from(is_integral || is_pointer || is_unscoped || is_floating)
        .to(BOOL_T)
        .build();
// const type_converter bool_to_any      = cmm::type_converter::builder().name("Bool to
// arithmetic").from(BOOL_T). BOOL_T >> &is_arithmetic;
const type_converter array_to_pointer =
    type_converter::builder().name("Array to pointer").from(is_array).with(get_pointer).build();
namespace {
type_converter::builder& const_lvalue_builder =
    type_converter::builder().from(is_category(type_category_t::lvalue_ref_t) && (is_const));
}
const type_converter const_lvalue_to_value =
    const_lvalue_builder.name("Const lvalue to value").with(get_underlying).build();
const type_converter lvalue_to_rvalue = type_converter::builder()
                                            .name("Lvalue to rvalue")
                                            .from((is_category(type_category_t::lvalue_ref_t)))
                                            .with(get_underlying)
                                            .build();

const std::array<const type_converter*, 6> standard = {&any_to_bool,
                                                       &lvalue_to_rvalue,
                                                       &const_lvalue_to_value,
                                                       &array_to_pointer};
} // namespace conversions

std::vector<ptype> get_convertible_types(ptype from) {
  bool c = 8 == 9.;
  return conversions::standard | std::views::filter([from](const type_converter* conv) {
           return conv->is_convertible(from);
         }) |
         std::views::transform(
             [&from](const type_converter* converter) { return (*converter)(from); }) |
         std::ranges::to<std::vector>();
}

bool is_convertible(ptype from, ptype to) {
  if (from == to) {
    return true;
  }
  auto converted =
      get_convertible_types(from) | std::views::filter([to](ptype t) { return t == to; });
  return !std::ranges::empty(converted);
}

} // namespace cmm
