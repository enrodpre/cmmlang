#pragma once

#include "token.hpp"

namespace cmm {

constexpr token_t::properties::properties(pattern_t t,
                                          cstring p,
                                          type_t ty) noexcept
    : pattern_type(t),
      pattern(p),
      type(ty) {}

constexpr const token_t::properties_map& token_t::properties_array() {
  using enum token_t::properties::pattern_t;
  using enum _token_t;
  static const token_t::properties_map MAP{
      {semicolon,    {SINGLE_CHAR, ";"}                                      },
      {o_paren,      {SINGLE_CHAR, "("}                                      },
      {c_paren,      {SINGLE_CHAR, ")"}                                      },
      {o_curly,      {SINGLE_CHAR, "{"}                                      },
      {c_curly,      {SINGLE_CHAR, "}"}                                      },
      {o_bracket,    {SINGLE_CHAR, "["}                                      },
      {c_bracket,    {SINGLE_CHAR, "]"}                                      },
      {o_attr,       {MULTI_CHAR, "[["}                                      },
      {c_attr,       {MULTI_CHAR, "]]"}                                      },
      {plus,         {SINGLE_CHAR, "+"}                                      },
      {minus,        {SINGLE_CHAR, "-"}                                      },
      {star,         {SINGLE_CHAR, "*"}                                      },
      {fslash,       {SINGLE_CHAR, "/"}                                      },
      {comma,        {SINGLE_CHAR, ","}                                      },
      {assign,       {SINGLE_CHAR, "="}                                      },
      {ampersand,    {SINGLE_CHAR, "&"}                                      },
      {int_t,        {MULTI_CHAR, "int", type_t::int_t}                      },
      {float_t,      {MULTI_CHAR, "float", type_t::float_t}                  },
      {char_t,       {MULTI_CHAR, "char", type_t::char_t}                    },
      {if_,          {MULTI_CHAR, "if"}                                      },
      {else_,        {MULTI_CHAR, "else"}                                    },
      {while_,       {MULTI_CHAR, "while"}                                   },
      {for_,         {MULTI_CHAR, "for"}                                     },
      {goto_,        {MULTI_CHAR, "goto"}                                    },
      {break_,       {MULTI_CHAR, "break"}                                   },
      {continue_,    {MULTI_CHAR, "continue"}                                },
      {return_,      {MULTI_CHAR, "return"}                                  },
      {operator_,    {MULTI_CHAR, "operator"}                                },
      {static_,      {MULTI_CHAR, "static"}                                  },
      {add_a,        {MULTI_CHAR, "+="}                                      },
      {sub_a,        {MULTI_CHAR, "-="}                                      },
      {mul_a,        {MULTI_CHAR, "*="}                                      },
      {div_a,        {MULTI_CHAR, "/="}                                      },
      {eq,           {MULTI_CHAR, "=="}                                      },
      {neq,          {MULTI_CHAR, "!="}                                      },
      {lt,           {MULTI_CHAR, "<"}                                       },
      {le,           {MULTI_CHAR, "=<"}                                      },
      {gt,           {MULTI_CHAR, ">"}                                       },
      {ge,           {MULTI_CHAR, "=>"}                                      },
      {inc,          {MULTI_CHAR, "++"}                                      },
      {dec,          {MULTI_CHAR, "--"}                                      },
      {bool_lit,     {REGEX, "(true|false)", type_t::bool_t}                 },
      {int_lit,      {REGEX, "([-+]?[0-9]+(?=[^.]))", type_t::int_t}         },
      {char_lit,     {REGEX, "('.')", type_t::char_t}                        },
      {str_lit,      {REGEX, "(\".*\")", type_t::char_t}                     },
      {float_lit,
       {REGEX, "([+-]?([0-9]+[.][0-9]*)|([0-9]*[.][0-9]+))", type_t::float_t}},
      {label,        {REGEX, "([A-Za-z_\\-]+[A-Za-z_\\-0-9]*):"}             },
      {ident,        {REGEX, "([A-Za-z_\\-]+[A-Za-z_\\-0-9]*)"}              },
      // DEBUG
      {debug_ast,    {MULTI_CHAR, ":AST"}                                    },
      {debug_mem,    {MULTI_CHAR, ":MEM"}                                    },
      {debug_state,  {MULTI_CHAR, ":STATE"}                                  },
      {debug_tokens, {MULTI_CHAR, ":TOKENS"}                                 }
  };

  return MAP;
}

template <typename... Args>
  requires std::is_constructible_v<token, Args...>
void tokens::emplace_back(Args&&... args) {
  m_tokens.emplace<token>(std::forward<Args>(args)...);
}
} // namespace cmm
