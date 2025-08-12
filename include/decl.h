#pragma once

#include "ast.hpp"
#include <cstdint>
#include <string>

namespace cmm {
namespace decl {

  enum class declaration_t : uint8_t {
    translation_unit,
    variable,
    function,
    label,
    block,
    function_definition,
    namespace_,

  };

  class node;
  class scope;
  class declaration;
  class translation_unit;
  class variable;

  using variable_key = std::string;

  class scope {};
  class declaration {
    declaration_t m_declaration_type;
  };

  class variable : declaration {};

}; // namespace decl
} // namespace cmm
