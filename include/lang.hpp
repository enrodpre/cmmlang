#pragma once

#include <cstddef>
#include <cstdint>
#include <magic_enum/magic_enum.hpp>
#include <magic_enum/magic_enum_containers.hpp>
#include <magic_enum/magic_enum_format.hpp>
#include <magic_enum/magic_enum_switch.hpp>
#include <string>
#include <string_view>
#include <sys/types.h>
#include <type_traits>
#include <utility>

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

  static mangled_name variable(std::string_view, types::type_id);
  static mangled_name label(std::string_view);
  static mangled_name function(std::string_view, const std::vector<types::type_id>&);
  static std::string types(const std::vector<types::type_id>&);

  [[nodiscard]] const value_type& str() const;
  operator std::string() const;

private:
  value_type m_string;
};

enum class binding_mode_t : u_int8_t { DIRECT, COPY, TEMPORARY, MOVE };

struct callable_signature : displayable {
  std::string name;
  std::vector<types::type_id> argument_types;

  callable_signature(std::string_view, std::vector<types::type_id>);

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
  types::type_id return_type;
  callable_contract(types::type_id ret, std::string_view n, std::vector<types::type_id> types)
      : callable_signature(n, std::move(types)),
        return_type(std::move(ret)) {}
  bool operator==(const callable_contract& other) const {
    return return_type == other.return_type && callable_signature::operator==(other);
  }
};

struct parameter {
  types::type_id type;
  std::string identifier;
  ast::expr::expression* init;
  const ast::decl::variable* decl;
  parameter(types::type_id t, std::string id, decltype(init) i, decltype(decl) d)
      : type(std::move(t)),
        identifier(std::move(id)),
        init(i),
        decl(d) {}
  parameter(types::type_id t, std::string id)
      : parameter(std::move(t), std::move(id), nullptr, nullptr) {}
};

using bound_argument = std::pair<parameter, binding_mode_t>;

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
using header_arguments_t = std::vector<types::type_id>;

struct builtin_signature_data : public cmm::enumeration<builtin_signature_t>, public callable {
  using value_type   = builtin_signature_t;
  using element_type = builtin_signature_data;
  using enum value_type;
  const value_type self;
  std ::string_view function_name;
  header_arguments_t args;
  using member_types   = std ::tuple<value_type, std ::string_view, header_arguments_t>;
  using properties_map = magic_enum ::containers ::array<value_type, member_types>;
  [[nodiscard]] std ::string string() const override { return std::format("{}", function_name); }
  [[nodiscard]] static constexpr const properties_map& properties_array();
  constexpr builtin_signature_data(value_type e)
      : self(e),
        function_name(std ::get<0 + 1>(element_type ::properties_array().at(e))),
        args(std ::get<1 + 1>(element_type ::properties_array().at(e))) {};
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
  types::type_id ret;
  std::vector<types::type_id> params;
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

struct conversor : displayable {
  conversor(std::string d,
            types::unary_matcher t_from,
            types::unary_matcher t_to,
            types::modifier t_modifier)
      : m_desc(d),
        m_from(t_from),
        m_to(t_to),
        m_modifier(t_modifier) {}
  [[nodiscard]] bool is_convertible(types::type_id) const;
  type_id operator()(type_id) const;
  [[nodiscard]] std::string string() const override { return m_desc; }

private:
  std::string m_desc;
  types::unary_matcher m_from;
  types::unary_matcher m_to;
  types::modifier m_modifier;
  // bool m_explicit = false;
  std::vector<instruction> m_instructions;
};

#define CONVERSION(NAME, FROM, TO, MOD) inline const auto& NAME = conversor{#NAME, FROM, TO, MOD};

CONVERSION(lvalue_to_rvalue,
           types::is_lvalue,
           types::is_rvalue,
           types::remove_lvalue | types::add_rvalue_reference);
CONVERSION(any_to_bool,
           types::is_integral || types::is_pointer || types::is_unscoped || types::is_floating,
           types::is_bool,
           types::replace(BOOL_T));
// CONVERSION(nullptr_to_anyptr, types::is_nullptr, types::is_pointer, )

inline const std::array standard_conversions = {&any_to_bool, &lvalue_to_rvalue};

extern std::vector<types::type_id> get_convertible_types(types::type_id);
extern bool is_convertible(types::type_id, types::type_id);

inline static const magic_enum::containers::
    array<value_category_t, std::vector<std::pair<types::unary_matcher, binding_mode_t>>>
        binding_rules{{{// LVALUE
                        {{types::is_direct, binding_mode_t::COPY},
                         {types::is_lvalue, binding_mode_t::DIRECT}},
                        // PRVALUE
                        {{types::is_const_lvalue, binding_mode_t::TEMPORARY},
                         {types::is_rvalue || types::is_direct, binding_mode_t::DIRECT}},
                        // XVALUE
                        {{types::is_const_lvalue, binding_mode_t::TEMPORARY},
                         {types::is_direct, binding_mode_t::MOVE},
                         {types::is_rvalue, binding_mode_t::DIRECT}}}}};

extern bool is_bindable_to(value_category_t, types::type_id);
extern binding_mode_t bind_value(value_category_t, types::type_id);
extern value_category_t get_value_category(types::type_id);

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
  constexpr static size_t operator()(const types::type_id&);
  constexpr static size_t operator()(const object&);
};

struct align {};

struct object {
  std::string name;
  align alignment;
  storage_t storage;
  types::type_id type;
  cmm::value* value;
};

constexpr size_t cmm::sizeof_::operator()(const types::type_id& t) {
  using enum types::group_t;
  using enum types::core_t;
  using enum types::layer_t;

  t->categorize();
  // if (cat == lvalue) {}
  //   switch (t->categorize().inner()) {
  //     case lvalue_ref_t:
  //     case rvalue_ref_t:
  //     case nullptr_t:
  //     case pointer_t:
  //       return 8;
  //     case bool_t:
  //       return 1;
  //     case char_t:
  //       return 1;
  //     case uint_t:
  //     case sint_t:
  //       return 4;
  //     case float_t:
  //       return 8;
  //     case array_t:
  //     case function_t:
  //     case void_t:
  //     case scoped_enum_t:
  //     case unscoped_enum_t:
  //     case class_t:
  //       return 0;
  //     case any_t:
  //     case fundamental_t:
  //     case arithmetic_t:
  //     case integral_t:
  //     case compound_t:
  //     case indirection_t:
  //     case reference_t:
  //     case enum_t:
  //     default:
  //       break;
  //   }
  return 0;
}

constexpr size_t cmm::sizeof_::operator()(const object& o) { return sizeof_::operator()(o.type); }

} // namespace cmm

#include "lang.inl"
