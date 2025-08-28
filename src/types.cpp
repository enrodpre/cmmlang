#include "types.hpp"

#include <format>

namespace cmm {
namespace types {

core::core(core_t t)
    : kind(t),
{}
core::core(category_t cat)
    : kind(core_t::Builtin) {
  auto cat_name = magic_enum::enum_name(cat);
  name          = cat_name.substr(cat_name.size() - 2);
}

const info* type_id::operator->() const { return &arena().id(*this); }
types::info type_id::info() const { return arena().id(*this); }

category_t categorize(type_id t) {
  if (t->layers.size() == 0) {
    auto name = t->core.name;
    return magic_enum::enum_cast<category_t>(name.substr(name.size() - 2)).value();
  }
  auto outer_fold = t->layers.top();
  return magic_enum::enum_cast<category_t>(magic_enum::enum_name(outer_fold.tag)).value();
}

size_t core_keyhash::operator()(const std::pair<core, std::vector<layer>>& k) const noexcept {
  // Simple FNV-like fold
  auto h = std::hash<int>{}(static_cast<int>(k.first.kind));
  h ^= std::hash<std::string>{}(k.first.name) + 0x9e3779b9 + (h << 6) + (h >> 2);
  for (auto& L : k.second) {
    size_t lh = std::hash<int>{}(static_cast<int>(L.tag));
    if (L.rank != 0)
      lh ^= std::hash<size_t>{}(L.rank + 0x9e37);
    h ^= lh + 0x9e3779b9 + (h << 6) + (h >> 2);
  }
  return h;
}

bool core_keyeq::operator()(const std::pair<core, std::vector<layer>>& a,
                            const std::pair<core, std::vector<layer>>& b) const noexcept {
  if (!(a.first == b.first))
    return false;
  if (a.second.size() != b.second.size())
    return false;
  for (size_t i = 0; i < a.second.size(); ++i)
    if (!(a.second[i] == b.second[i]))
      return false;
  return true;
}
match_result& match_result::operator&&(const match_result& other) {
  ok &= other.ok;
  return *this;
}

match_result& match_result::operator||(const match_result& other) {
  ok |= other.ok;
  return *this;
}

match_result& match_result::operator!() {
  this->ok = !this->ok;
  return *this;
}

matcher::matcher(std::string desc, matcher_t t)
    : m_desc(desc),
      m_matcher(t) {}

match_result matcher::operator()(type_id t) const { return m_matcher(t); }

std::string matcher::string() const { return m_desc; }

matcher matcher::operator&&(const matcher& m) const {
  return {std::format("{} and {}", *this, m),
          [*this, m](type_id t) -> match_result { return this->m_matcher(t) && m.m_matcher(t); }};
}

matcher matcher::operator||(const matcher& m) const {
  return {std::format("{} or {}", *this, m),
          [*this, m](type_id t) -> match_result { return this->m_matcher(t) || m.m_matcher(t); }};
}

matcher matcher::operator!() const {
  return {std::format("not {}", *this),
          [this](type_id t) -> match_result { return !this->m_matcher(t); }};
}

bool belongs_to(const category_data& child, category_t parent) {
  if (parent == child.self) {
    return true;
  }
  if (child.self == category_t::generic_t || child.self == category_t::dummy_t) {
    return false;
  }
  if (parent == category_t::any_t) {
    return true;
  }
  if (child.self == category_t::any_t) {
    return false;
  }
  return belongs_to(child.parent, parent);
}

type_id modifier::operator()(type_id t) const { return arena().get(m_modifier(t.info())); }
info modifier::operator()(info i) const { return m_modifier(i); }

modifier modifier::operator|(const modifier& other) const {
  return {std::format("{}, {}", m_desc, other.m_desc),
          [=, *this](info t) -> info { return other(m_modifier(t)); }};
}

// bool converter::is_convertible(type t) const { return m_from->match(t); }

// type converter::operator()(type t) const {
//   if (!is_convertible(t)) {
//     throw error(std::format("Type {} is not convertible with {}", t, *this));
//   }
//   return m_modifier(t);
// }

// std::vector<type> get_convertible_types(type from) {
//   return conversions::standard |
//          std::views::filter([from](const converter* conv) { return conv->is_convertible(from); })
//          | std::views::transform([&from](const converter* converter) { return (*converter)(from);
//          }) | std::ranges::to<std::vector>();
// }
//
// bool is_convertible(type from, type to) {
//   if (from == to) {
//     return true;
//   }
//   auto converted =
//       get_convertible_types(from) | std::views::filter([to](type t) { return t == to; });
//   return !std::ranges::empty(converted);
// }
// std::string type::string() const {
//   switch (category) {
//     case category_t::void_t:
//       return "void";
//     case category_t::nullptr_t:
//       return "nullptr_t";
//     case category_t::bool_t:
//       return std::format("{}{}bool", c ? "c" : "", v ? "v" : "");
//     case category_t::char_t:
//       return std::format("{}{}char", c ? "c" : "", v ? "v" : "");
//     case category_t::uint_t:
//       return std::format("{}{}uint", c ? "c" : "", v ? "v" : "");
//     case category_t::sint_t:
//       return std::format("{}{}sint", c ? "c" : "", v ? "v" : "");
//     case category_t::float_t:
//       return std::format("{}{}float", c ? "c" : "", v ? "v" : "");
//     case category_t::lvalue_ref_t:
//       return std::format("{}&{}{}", underlying->string(), c ? "c" : "", v ? "v" : "");
//     case category_t::rvalue_ref_t:
//       return std::format("{}&&{}{}", underlying->string(), c ? "c" : "", v ? "v" : "");
//     case category_t::pointer_t:
//       return std::format("{}*{}{}", underlying->string(), c ? "c" : "", v ? "v" : "");
//     case category_t::array_t:
//       return std::format("<{}, {}>", underlying->string(), rank);
//     case category_t::scoped_enum_t:
//     case category_t::unscoped_enum_t:
//       return std::format("<{}, {}>", underlying->string(), rank);
//     case category_t::class_t:
//       return std::format("{}", underlying->string());
//     case category_t::function_t:
//     default:
//       return "";
//   }
// }
}; // namespace types
} // namespace cmm
