#include "lang.hpp"
#include "ast.hpp"
#include "types.hpp"
#include <sys/types.h>

// std::size_t cmm::type_info::hash() const {
//   return std::hash<cmm::type_info>{}(*this);
// }
namespace cmm {

[[nodiscard]] ast::decl::signature builtin_signature_data::signature() const {
  return {ast::identifier(std::string(function_name)), args};
}

mangled_name mangled_name::function(cstring name, const std::vector<const type*>& t) {
  return std::format("{}_{}", name, types(t));
}

std::string mangled_name::types(const std::vector<ptr_type>& types) {
  return types | std::views::transform([](ptr_type t) { return t->format(); }) |
         std::views::join_with('_') | std::ranges::to<std::string>();
}

mangled_name mangled_name::direct_conversion_function(cr_type f, cr_type t) {
  return function("conv_{}", std::vector<ptr_type>{&f, &t});
}

mangled_name mangled_name::label(cstring name) { return std::string(name); }
mangled_name mangled_name::variable(cstring name, cr_type) { return std::string(name); }

mangled_name::operator std::string() const { return m_string; }
// constexpr type_t typeof ::operator()(type) { return type_t::nullptr_t; }

// constexpr category_t typeof ::operator()(const type& t) { return t.category; }
constexpr size_t sizeof_::operator()(const type& t) {
  using enum type_category_t;
  switch (t.category) {
    case lvalue_ref_t:
    case rvalue_ref_t:
    case nullptr_t:
    case pointer_t:
      return 8;
    case bool_t:
      return 1;
    case char_t:
      return 1;
    case uint_t:
    case sint_t:
      return 4;
    case float_t:
      return 8;
    case array_t:
    case function_t:
    case void_t:
    case scoped_enum_t:
    case unscoped_enum_t:
    case class_t:
      return 0;
    case type_category_t::any_t:
    case type_category_t::fundamental_t:
    case type_category_t::arithmetic_t:
    case type_category_t::integral_t:
    case type_category_t::compound_t:
    case type_category_t::indirection_t:
    case type_category_t::reference_t:
    case type_category_t::enum_t:
    default:
      break;
  }
  return 0;
}
constexpr size_t sizeof_::operator()(const object& o) { return sizeof_::operator()(o.type); }
} // namespace cmm
