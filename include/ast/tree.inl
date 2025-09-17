#pragma once

#include "ast/tree.hpp"

namespace cmm::ast {
inline const magic_enum::containers::
    array<value_category_t, std::vector<std::pair<types::unary_matcher, binding_mode_t>>>
        translation_unit::binding_rules{
            {{// LVALUE
              {{types::is_direct, binding_mode_t::COPY},
               {types::is_lvalue, binding_mode_t::DIRECT}},
              // PRVALUE
              {{types::is_const_lvalue, binding_mode_t::TEMPORARY},
               {types::is_rvalue || types::is_direct, binding_mode_t::DIRECT}},
              // XVALUE
              {{types::is_const_lvalue, binding_mode_t::TEMPORARY},
               {types::is_direct, binding_mode_t::MOVE},
               {types::is_rvalue, binding_mode_t::DIRECT}}}}};

} // namespace cmm::ast
