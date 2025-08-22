#pragma once

#include <cstddef>
#include <cstdint>
#include <format>
#include <magic_enum/magic_enum.hpp>
#include <magic_enum/magic_enum_containers.hpp>
#include <magic_enum/magic_enum_format.hpp>
#include <ranges>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "common.hpp"
#include "macros.hpp"
#include "types.hpp"

namespace cmm {

namespace assembly {
class operand;
}
namespace ast::decl {
class function;
class signature;
} // namespace ast::decl
class mangled_name {

public:
  using value_type = std::string;
  mangled_name(const value_type& v)
      : m_string(v) {}
  mangled_name(value_type&& v)
      : m_string(std::move(v)) {}

  static mangled_name variable(cstring, crtype);
  static mangled_name label(cstring);
  static mangled_name function(cstring, const std::vector<ptype>&);
  static mangled_name direct_conversion_function(crtype, crtype);
  static std::string types(const std::vector<const type*>&);

  [[nodiscard]] const value_type& str() const;
  operator std::string() const;

private:
  value_type m_string;
};

template <typename T>
concept Operand = (std::is_same_v<assembly::operand*, T> || std::is_same_v<arg_t, T>);

struct instruction {
  instruction_t ins;
  arg_t arg1;
  arg_t arg2;
};

struct signature {
  std::string name;
  std::vector<ptype_spec> argument_types;

  signature(operator_t, std::vector<ptype_spec>);
  bool operator==(const signature& other) const {
    return name == other.name &&
           std::ranges::all_of(std::views::zip(argument_types, other.argument_types),
                               [](const auto& type_pair) {
                                 const auto& [t, other_t] = type_pair;
                                 return t == other_t;
                               });
  }
};

enum class builtin_signature_t : uint8_t { MAIN, SYSCALL, EXIT, PRINT };
using header_arguments_t = std::vector<ptype>;
struct builtin_signature_data : public cmm::enumeration<builtin_signature_t>, public displayable {
  BUILD_ENUMERATION_DATA(builtin_signature,
                         std::string_view,
                         function_name,
                         header_arguments_t,
                         args);
  [[nodiscard]] ast::decl::signature signature() const;
};

struct operator_builtin_data {
  std::unique_ptr<type_specifier> ret;
  std::unique_ptr<type_specifier> arg1;
  std::unique_ptr<type_specifier> arg2;
  std::vector<instruction> ins;
  operator_builtin_data(ptype c, ptype b, ptype a, decltype(ins)&& e)
      : ret(std::make_unique<type>(*c)),
        arg1(std::make_unique<type>(*b)),
        arg2(std::make_unique<type>(*a)),
        ins(std::move(e)) {}
  operator_builtin_data(type_converter c,
                        const type_matcher& first,
                        const type_matcher& second,
                        decltype(ins)&& e)
      : ins(std::move(e)) {
    type_placeholder p(first);
    arg1 = p.create_dependant(p);
  }
};

extern const std::unordered_map<operator_t, operator_builtin_data> builtin_operators;
static_assert(std::formattable<cmm::instruction_t, char>);

std::optional<operator_builtin_data> get_builtin_operator(operator_t, const std::vector<ptype>&);

enum class attribute : uint8_t {
  no_return          = 1 << 1,
  carries_dependency = 1 << 2,
  deprecated         = 1 << 3,
};

enum class linkage_t : uint8_t { normal = 0, internal, external };
enum class storage_t : uint8_t { normal = 0, static_, extern_, mutable_, register_ };

enum class modifier_t : uint8_t {
  friend_,
  constexpr_,
  const_,
  volatile_,
  ptr,
  ref,
  signed_,
  unsigned_,
  constinit_,
  consteval_,
};

template <typename T> struct storage {
  virtual T& stored() const = 0;
};
template <typename T> struct static_storage : public storage<T> {};
template <typename T> struct dynamic_storage : public storage<T> {
  uintptr_t address;
};

#define DEFINE_STATIC_SIZE(TYPE, VALUE) \
  template <> struct sizeof_<TYPE> { \
    static constexpr size_t value = VALUE; \
  };

struct value {};
struct scalar_value : public value {
  // type_t type;
};
struct composite_value : public value {
  size_t length;
};
struct object;

struct sizeof_ {
  STATIC_CLS(sizeof_);
  constexpr static size_t operator()(const type&);
  constexpr static size_t operator()(const object&);
};

struct align {};

struct object {
  std::string name;
  align alignment;
  storage_t storage;
  cmm::type type;
  cmm::value* value;
};

constexpr size_t cmm::sizeof_::operator()(const type& t) {
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
constexpr size_t cmm::sizeof_::operator()(const object& o) { return sizeof_::operator()(o.type); }
} // namespace cmm
#include "lang.inl"
