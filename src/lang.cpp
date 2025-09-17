#include "lang.hpp"

#include "ast/tree.hpp"
#include "common.hpp"
#include "types.hpp"

using namespace cmm;
using cmm::builtin_callable;
using cmm::value_category_t;
using cmm::types::type_id;

cmm::ast::identifier builtin_callable::identifier() const {
  return {mangle_function(name,
                          params | std ::views ::transform([](const parameter& t_param) {
                            return t_param.type->string();
                          }) | TO_VEC,
                          '_')};
}

[[nodiscard]] cmm::parameters builtin_callable::parameters_impl() const { return params; }

value_category_t cmm::get_value_category(type_id t) {
  if (cmm::types::is_lvalue(t).ok) {
    return value_category_t::LVALUE;
  }
  if (cmm::types::is_rvalue(t).ok) {
    return value_category_t::XVALUE;
  }
  return value_category_t::PRVALUE;
}

const conversor cmm::lvalue_to_rvalue =
    conversor{"lvalue_to_rvalue",
              [](value_category_t t_cat, type_id t_type) {
                auto mode = ast::translation_unit::bind_value(t_cat, t_type);
                using magic_enum::enum_fuse;
                using enum value_category_t;
                using enum binding_mode_t;
                switch (magic_enum::enum_fuse(t_cat, mode).value()) {
                  case enum_fuse(LVALUE, COPY).value():
                    return true;
                  default:
                    return false;
                }
              },
              types::type_identity,
              [](value_category_t t_cat) {
                if (t_cat == value_category_t::LVALUE) {
                  return value_category_t::PRVALUE;
                }
                return t_cat;
              }};
const conversor cmm::any_to_bool = conversor{"any_to_bool",
                                             [](value_category_t, type_id t_type) {
                                               auto t_matcher =
                                                   types::is_integral || types::is_pointer ||
                                                   types::is_unscoped || types::is_floating;
                                               return t_matcher(t_type);
                                             },
                                             types::replace(BOOL_T),
                                             std::identity{}};
