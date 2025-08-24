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
#include <sys/types.h>
#include <type_traits>
#include <utility>
#include <vector>

#include "common.hpp"
#include "macros.hpp"
#include "types.hpp"

namespace cmm {

namespace assembly {
struct operand;
}

namespace ast {
namespace decl {
struct function;
struct variable;
} // namespace decl

namespace expr {
struct expression;
}
} // namespace ast
class mangled_name {

public:
  using value_type = std::string;

  mangled_name(const value_type& v)
      : m_string(v) {}

  mangled_name(value_type&& v)
      : m_string(std::move(v)) {}

  static mangled_name variable(cstring, crptype);
  static mangled_name label(cstring);
  static mangled_name function(cstring, const std::vector<ptype>&);
  static std::string types(const std::vector<ptype>&);

  [[nodiscard]] const value_type& str() const;
  operator std::string() const;

private:
  value_type m_string;
};

struct callable_signature : displayable {
  std::string name;
  std::vector<ptype> argument_types;

  callable_signature(cstring, std::vector<ptype>);

  [[nodiscard]] std::string string() const override {
    return mangled_name::function(name, argument_types);
  }

  bool operator==(const callable_signature& other) const {
    return name == other.name &&
           std::ranges::all_of(std::views::zip(argument_types, other.argument_types),
                               [](const auto& type_pair) {
                                 const auto& [t, other_t] = type_pair;
                                 return t == other_t;
                               });
  }
};

struct callable_contract : public callable_signature {
  ptype return_type;
  callable_contract(ptype ret, cstring n, std::vector<ptype> types)
      : callable_signature(n, std::move(types)),
        return_type(std::move(ret)) {}
  bool operator==(const callable_contract& other) const {
    return return_type == other.return_type && callable_signature::operator==(other);
  }
};

struct parameter {
  // value_category_t value_category;
  ptype type;
  std::string identifier;
  ast::expr::expression* init;
  const ast::decl::variable* decl;
  parameter(ptype t, std::string id, decltype(init) i, decltype(decl) d)
      : type(std::move(t)),
        identifier(std::move(id)),
        init(i),
        decl(d) {}
  parameter(ptype t, std::string id)
      : parameter(std::move(t), std::move(id), nullptr, nullptr) {}
};

// struct bound_argument : public parameter {
//   assembly::operand* address;
//
//   bound_argument(ptype t,
//                  std::string id,
//                  decltype(init) i,
//                  decltype(decl) d,
//                  decltype(address) addr)
//       : parameter(std::move(t), std::move(id), i, d),
//         address(addr) {}
//   bound_argument(ptype t, std::string id, decltype(address) addr)
//       : parameter(std::move(t), std::move(id), nullptr, nullptr),
//         address(addr) {}
// };

using bound_argument = parameter;

struct callable {
  virtual ~callable()                                           = default;
  [[nodiscard]] virtual cmm::callable_contract contract() const = 0;
  [[nodiscard]] virtual cmm::callable_signature signature() const { return contract(); } // NOLINT
  [[nodiscard]] virtual std::vector<parameter> parameters() const = 0;
};

template <typename T>
concept Operand = (std::is_same_v<assembly::operand*, T> || std::is_same_v<arg_t, T>);

struct instruction {
  instruction_t ins;
  arg_t arg1;
  arg_t arg2;
};

enum class builtin_signature_t : uint8_t { MAIN, SYSCALL, EXIT, PRINT };
using header_arguments_t = std::vector<ptype>;

struct builtin_signature_data : public cmm::enumeration<builtin_signature_t>,
                                public displayable,
                                public callable {
  BUILD_ENUMERATION_DATA(builtin_signature,
                         std::string_view,
                         function_name,
                         header_arguments_t,
                         args);
  [[nodiscard]] cmm::callable_contract contract() const override;
  [[nodiscard]] std::vector<parameter> parameters() const override {
    return args | std::views::enumerate | std::views::transform([](const auto& pair) -> parameter {
             std::string name = std::format("undeclared_{}", std::get<0>(pair));
             return {std::get<1>(pair), name, nullptr, nullptr};
           }) |
           std::ranges::to<std::vector>();
  }
};

namespace ast {
struct operator_;
}

struct builtin_operator_data : callable {
  using identifier_t = ast::operator_;
  ptype ret;
  std::vector<ptype> params;
  std::vector<instruction> ins;

  builtin_operator_data(decltype(ret) c, decltype(ret) b, decltype(ins)&& e)
      : ret(std::move(c)),
        params({std::move(b)}),
        ins(std::move(e)) {}

  builtin_operator_data(decltype(ret) c, decltype(ret) b, decltype(ret) a, decltype(ins)&& e)
      : ret(std::move(c)),
        params({std::move(a), std::move(b)}),
        ins(std::move(e)) {}

  [[nodiscard]] callable_contract contract() const override { return {ret, "", params}; }
  [[nodiscard]] std::vector<parameter> parameters() const override {
    return params | std::views::enumerate |
           std::views::transform([](const auto& pair) -> parameter {
             std::string name = std::format("undeclared_{}", std::get<0>(pair));
             return {std::get<1>(pair), name};
           }) |
           std::ranges::to<std::vector>();
  }
};

struct builtin_operator {
  operator_t op;
  builtin_operator_data data;
};

extern const std::unordered_map<operator_t, std::vector<builtin_operator_data>> builtin_operators;
static_assert(std::formattable<cmm::instruction_t, char>);

std::vector<const builtin_operator_data*> get_builtin_operator(operator_t);

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

template <typename T>
struct storage {
  virtual T& stored() const = 0;
};

template <typename T>
struct static_storage : public storage<T> {};

template <typename T>
struct dynamic_storage : public storage<T> {
  uintptr_t address;
};

#define DEFINE_STATIC_SIZE(TYPE, VALUE)    \
  template <>                              \
  struct sizeof_<TYPE> {                   \
    static constexpr size_t value = VALUE; \
  };

enum class binding_aftermath_t : u_int8_t { DIRECT, COPY, TEMPORARY };

struct binding_rule {
  binding_aftermath_t aftermath;
  type_matcher matcher;
  type_modifier modifier;
};

inline static const magic_enum::containers::array<value_category_t, std::vector<binding_rule>>
    binding_rules{
        {{// LVALUE
          {{binding_aftermath_t::COPY, !matchers::is_ref, modifiers::identity},
           {binding_aftermath_t::DIRECT, !matchers::is_ref, modifiers::add_lvalue_reference},
           {binding_aftermath_t::DIRECT,
            !matchers::is_ref,
            modifiers::add_lvalue_reference | modifiers::add_const}},
          // PRVALUE
          {{binding_aftermath_t::COPY, !matchers::is_ref, modifiers::identity},
           {binding_aftermath_t::TEMPORARY,
            !matchers::is_ref,
            modifiers::add_lvalue_reference | modifiers::add_const},
           {binding_aftermath_t::DIRECT, !matchers::is_ref, modifiers::add_rvalue_reference}},
          // XVALUE
          {{binding_aftermath_t::COPY, matchers::is_rvalue, modifiers::remove_reference},
           {binding_aftermath_t::DIRECT,
            matchers::is_rvalue,
            modifiers::add_rvalue_reference | modifiers::add_const},
           {binding_aftermath_t::DIRECT, matchers::is_rvalue, modifiers::identity}}}}};

extern std::vector<ptype> get_bindable_candidates(value_category_t, crptype);
extern bool is_bindeable(value_category_t, crptype, crptype);
extern ptype bind_argument(value_category_t, crptype, crptype);
extern value_category_t get_value_category(crptype);

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
