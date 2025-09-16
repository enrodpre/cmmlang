#pragma once

#include "ast_visitor.hpp"
#include <type_traits>

namespace cmm::ast {
template <typename Node>
  requires(std::is_base_of_v<node, Node>)
void print_node(const Node& t_node) {}
} // namespace cmm::ast
