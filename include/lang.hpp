#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <magic_enum/magic_enum.hpp>
#include <magic_enum/magic_enum_containers.hpp>
#include <magic_enum/magic_enum_format.hpp>
#include <magic_enum/magic_enum_switch.hpp>
#include <string>
#include <sys/types.h>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

#include "common.hpp"
#include "macros.hpp"
#include "memory.hpp"
#include "types.hpp"

namespace cmm {

namespace assembly {
struct operand;
}

namespace ast {
struct identifier;
namespace decl {
struct function;
struct variable;
} // namespace decl

namespace expr {
struct expression;
}
} // namespace ast

enum class binding_mode_t : u_int8_t { DIRECT, COPY, TEMPORARY, MOVE };

using parameter      = ast::decl::variable;
using bound_argument = std::pair<parameter, binding_mode_t>;
using parameters = std::vector<ast::decl::variable, cmm::memory::allocator<ast::decl::variable>>;

struct callable {
  virtual ~callable()                                      = default;
  virtual ast::identifier identifier() const               = 0;
  [[nodiscard]] virtual cmm::parameters parameters() const = 0;
  [[nodiscard]] virtual type_id return_type() const        = 0;
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

namespace ast {
struct operator_;
}

struct builtin_callable : public callable {
  std::string name;
  types::type_id ret;
  std::vector<types::type_id> params;
  std::vector<instruction> ins;

  builtin_callable(operator_t op, decltype(ret) c, decltype(params) params, decltype(ins) e)
      : ret(c),
        params(params),
        ins(e) {}
  builtin_callable(std::string str, decltype(ret) c, decltype(params) params, decltype(ins) e)
      : ret(c),
        params(params),
        ins(e) {}

  ast::identifier identifier() const override;
  type_id return_type() const override { return ret; }
  [[nodiscard]] cmm::parameters parameters() const override;
};

extern const std::unordered_map<operator_t, std::vector<builtin_callable>> builtin_operators;

std::vector<const builtin_callable*> get_builtin_operator(operator_t);

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
using value_category_conversor_type = std::function<value_category_t(value_category_t)>;

struct conversor : displayable {
  using condition_type = std::function<bool(value_category_t, type_id)>;

  conversor(std::string d,
            condition_type t_condition,
            types::modifier t_type_modifier,
            value_category_conversor_type t_value_mod)
      : m_desc(d),
        m_condition(t_condition),
        m_type_modifier(t_type_modifier),
        m_value_category_modifier(t_value_mod) {}

  [[nodiscard]] bool is_convertible(value_category_t t_cat, types::type_id t_type) const {
    return m_condition(t_cat, t_type);
  };
  type_id convert_type(type_id t_type) const { return m_type_modifier(t_type); };
  value_category_t convert_value_category(value_category_t t_cat) const {
    return m_value_category_modifier(t_cat);
  }
  [[nodiscard]] std::string string() const override { return m_desc; }

private:
  std::string m_desc;
  condition_type m_condition;
  types::modifier m_type_modifier;
  value_category_conversor_type m_value_category_modifier;
  std::vector<instruction> m_instructions;
};

#define CONVERSION(NAME, COND, TYPE_MOD, TYPE_VAL)                      \
  inline const auto& NAME = conversor{#NAME, COND, TYPE_MOD, TYPE_VAL};

CONVERSION(
    lvalue_to_rvalue,
    [](value_category_t t_cat, type_id t_type) {
      auto mode = bind_value(t_cat, t_type);
      using magic_enum::enum_fuse;
      using enum value_category_t;
      using enum binding_mode_t;
      switch (magic_enum::enum_fuse(t_cat, mode).value()) {
        case enum_fuse(LVALUE, COPY).value():
          return true;
        case enum_fuse(LVALUE, DIRECT).value():
        case enum_fuse(PRVALUE, TEMPORARY).value():
        case enum_fuse(PRVALUE, DIRECT).value():
        default:
          return false;
      }
    },
    types::type_identity,
    [](value_category_t t_cat) {
      if (t_cat == value_category_t::LVALUE) {
        return value_category_t::RVALUE;
      }
      return t_cat;
    });

CONVERSION(
    any_to_bool,
    [](value_category_t, type_id t_type) {
      auto t_matcher =
          types::is_integral || types::is_pointer || types::is_unscoped || types::is_floating;
      return t_matcher(t_type);
    },
    types::replace(BOOL_T),
    std::identity{});
// CONVERSION(nullptr_to_anyptr, types::is_nullptr, types::is_pointer, )

inline const std::array standard_conversions = {&any_to_bool, &lvalue_to_rvalue};

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
