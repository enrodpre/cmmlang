#include "types.hpp"

#include <magic_enum/magic_enum.hpp>
#include <assert.h>
#include <format>
#include <utility>

#include "types.inl"

namespace cmm::types {

core::core(core_t t)
    : kind(t),
      name(magic_enum::enum_name(t)) {}

info::info(const types::core& t_core, cv_qualification_t t_cv, cmm::stack<layer> t_layers)
    : core(t_core),
      cv_qualifiers(t_cv),
      layers(t_layers) {}

const info* types::type_id::operator->() const { return &manager::instance().info(*this); }
types::type_id::operator const info&() const { return manager::instance().info(*this); }

namespace {
std::string get_cv_repr(cv_qualification_t cvqual) {
  switch (cvqual) {
    case cmm::types::cv_qualification_t::CONST:
      return "c";
    case cmm::types::cv_qualification_t::VOLATILE:
      return "v";
    case cmm::types::cv_qualification_t::CONST_VOLATILE:
      return "cv";
  }
  return "";
}
} // namespace

std::string info::string() const {
  std::string indirection_repr = "";
  if (!layers.empty()) {
    switch (layers.top().tag) {
      case layer_t::array_t:
        indirection_repr = "a" + std::to_string(layers.top().rank);
        break;
      case layer_t::pointer_t:
        indirection_repr = "p";
        break;
      case layer_t::lvalue_ref_t:
        indirection_repr = "ref";
        break;
      case layer_t::rvalue_ref_t:
        indirection_repr = "rref";
        break;
      case layer_t::function_t:
        indirection_repr = "fn";
        break;
    }
  }
  return std::format(
      "{}{}{}", core.name.substr(0, core.name.size() - 2), get_cv_repr(cvqual()), indirection_repr);
}
category_t info::categorize() const {
  if (layers.empty()) {
    return core.kind;
  }
  return layers.top().tag;
}

cv_qualification_t& info::cvqual() {
  if (layers.empty()) {
    return cv_qualifiers;
  }
  return layers.top().cv_qualifiers;
}

const cv_qualification_t& info::cvqual() const {
  if (layers.empty()) {
    return cv_qualifiers;
  }
  return layers.top().cv_qualifiers;
}

size_t core_keyhash::operator()(const info::key_type& k) const noexcept {
  const auto& [core, cv, layers] = k;
  auto h                         = std::hash<int>{}(static_cast<int>(core.kind));
  h ^= std::to_underlying(cv) + 0x9e3779b9 + (h << 6) + (h >> 2);
  for (auto& L : layers) {
    size_t lh = std::hash<int>{}(static_cast<int>(L.tag));
    if (L.rank != 0)
      lh ^= std::hash<size_t>{}(L.rank + 0x9e37);
    h ^= lh + 0x9e3779b9 + (h << 6) + (h >> 2);
  }
  return h;
}

bool core_keyeq::operator()(const info::key_type& a, const info::key_type& b) const noexcept {
  return a == b;
}

types::type_id manager::make(const types::core& c,
                             const stack<layer>& layers,
                             cv_qualification_t qual) {
  auto key = std::make_tuple(c, qual, layers);
  auto it  = m_idx.find(key);
  if (it != m_idx.end())
    return types::type_id{it->second};
  uint32_t id = types_size();
  m_nodes.emplace_back(c, qual, layers);
  m_idx.emplace(std::move(key), id);
  return types::type_id{id};
}

types::type_id manager::get(types::info i) { return make(i.core, i.layers, i.cv_qualifiers); }

const info& manager::info(types::type_id t) const {
  assert(t.value() < m_nodes.size());
  return m_nodes[t.value()];
}

size_t manager::types_size() const { return m_nodes.size(); }

template <layer_t L>
modifier manager::make_layer_adder() {
  return {magic_enum::enum_name<L>(), [this](type_id t) -> type_id {
            types::info i = this->info(t);
            i.layers.emplace(L);
            return get(i);
          }};
}
template <cv_qualification_t L>
modifier manager::make_cv_adder() {
  return {magic_enum::enum_name<L>(), [this](type_id t) -> type_id {
            types::info i = info(t);
            i.cvqual() |= L;
            return get(i);
          }};
}
template <auto L>
  requires(CategoryValue<L>)
modifier manager::make_layer_remover() {
  return {std::format("remove {}", magic_enum::enum_name<L>()), [this](type_id t) -> type_id {
            if (is_category<L>(t)) {
              types::info i = info(t);
              i.layers.pop();
              return get(i);
            }
            return t;
          }};
}
template <cv_qualification_t L>
modifier manager::make_cv_remover() {
  return {magic_enum::enum_name<L>(), [this](type_id t) -> type_id {
            types::info i = info(t);
            i.cvqual() &= ~L;
            return get(i);
          }};
}

// match_result& match_result::operator&&(const match_result& other) {
//   ok &= other.ok;
//   return *this;
// }
//
// match_result& match_result::operator||(const match_result& other) {
//   ok |= other.ok;
//   return *this;
// }
//
// match_result& match_result::operator!() {
//   this->ok = !this->ok;
//   return *this;
// }
//
unary_matcher::unary_matcher(std::string_view desc, unary_matcher_t t)
    : m_desc(desc),
      m_unary_matcher(t) {}

match_result unary_matcher::operator()(types::type_id t) const { return m_unary_matcher(t); }

std::string unary_matcher::string() const { return m_desc; }

unary_matcher unary_matcher::operator&&(const unary_matcher& m) const {
  return {std::format("{} and {}", *this, m), [*this, m](types::type_id t) -> match_result {
            return this->m_unary_matcher(t) && m.m_unary_matcher(t);
          }};
}

unary_matcher unary_matcher::operator||(const unary_matcher& m) const {
  return {std::format("{} or {}", *this, m), [*this, m](types::type_id t) -> match_result {
            return this->m_unary_matcher(t) || m.m_unary_matcher(t);
          }};
}

unary_matcher unary_matcher::operator!() const {
  return {std::format("not {}", *this),
          [this](types::type_id t) -> match_result { return !this->m_unary_matcher(t); }};
}

binary_matcher::binary_matcher(std::string_view s, binary_matcher_t b)
    : m_desc(s),
      m_matcher(b) {}

match_result binary_matcher::operator()(type_id lhs, type_id rhs) const {
  return m_matcher(lhs, rhs);
}

unary_matcher binary_matcher::operator()(type_id lhs) const {
  return {std::format("{} {}", m_desc, lhs->string()),
          [lhs, this](type_id rhs) { return m_matcher(rhs, lhs); }};
}

std::string binary_matcher::string() const { return m_desc; }

bool belongs_to(category_t child, category_t parent) {
  // category_data child_data = child->this;
  if (parent == child) {
    return true;
  }
  if (parent == group_t::any_t) {
    return true;
  }
  if (child == group_t::any_t) {
    return false;
  }
  return belongs_to(category_t{child->parent}, parent);
}

types::type_id modifier::operator()(types::type_id t) const { return m_modifier(t); }

modifier modifier::operator|(const modifier& other) const {
  return {std::format("{} | {}", m_desc, other.m_desc),
          [other, this](type_id t) -> type_id { return other(operator()(t)); }};
}

// modifier modifier::operator^(const modifier& other) const {
//   return {std::format("{} xor {}", m_desc, other.m_desc), [other, this](type_id t) -> type_id {
//             if (m_condition(t)) {
//               return operator()(t);
//             }
//             return other(t);
//           }};
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
} // namespace cmm::types
