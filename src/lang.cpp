#include "lang.hpp"

#include "ast.hpp"
#include "common.hpp"
#include "memory.hpp"
#include "types.hpp"

using namespace cmm;
using cmm::builtin_callable;
using cmm::operator_t;
using cmm::value_category_t;
using cmm::types::type_id;

cmm::ast::identifier builtin_callable::identifier() const {
  return {mangle_function(name, params | MAP_RANGE(type_id, elem->string()), '_')};
}

[[nodiscard]] cmm::parameters builtin_callable::parameters() const {
  return params | std::views::enumerate |
         std::views::transform([](const auto& pair) -> ast::decl::variable {
           std::string name = std::format("var{}", std::get<0>(pair));
           return {std::get<1>(pair), ast::identifier(name), nullptr};
         }) |
         std::ranges::to<
             std::vector<ast::decl::variable, cmm::memory::allocator<ast::decl::variable>>>();
}

const std::unordered_map<cmm::operator_t, std::vector<builtin_callable>> cmm::builtin_operators{};

std::vector<const builtin_callable*> cmm::get_builtin_operator(operator_t op) {
  auto pointer_of = [](const builtin_callable& v) -> const builtin_callable* { return &v; };
  return cmm::builtin_operators.at(op) | TRANSFORM(pointer_of) | TO_VEC;
}

value_category_t cmm::get_value_category(type_id t) {
  if (cmm::types::is_lvalue(t).ok) {
    return value_category_t::LVALUE;
  }
  if (cmm::types::is_rvalue(t).ok) {
    return value_category_t::XVALUE;
  }
  return value_category_t::PRVALUE;
}

bool cmm::is_bindable_to(value_category_t value_category, type_id type) {
  return std::ranges::any_of(binding_rules.at(value_category),
                             [type](const auto& rule) { return rule.first(type).ok; });
}

cmm::binding_mode_t cmm::bind_value(value_category_t cat, type_id type) {
  auto mode = binding_rules.at(cat) |
              FILTER([type](const auto& rule) { return rule.first(type).ok; }) |
              std::views::values | TO_VEC;
  if (mode.empty()) {
    THROW(NOT_BINDEABLE, type, cat);
  }
  if (mode.size() > 1) {
    REGISTER_WARN("Too many results when binding values, {}", mode);
  }
  return mode.front();
}
