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
#include <utility>
#include <vector>

#include "common.hpp"
#include "macros.hpp"
#include "memory.hpp"
#include "types.hpp"

namespace cmm {

namespace assembly {
struct element;
}

namespace ast {
struct identifier;
struct translation_unit;
namespace decl {
struct function;
struct variable;
} // namespace decl

namespace expr {
struct expression;
}
} // namespace ast

enum class binding_mode_t : u_int8_t { DIRECT, COPY, TEMPORARY, MOVE };

struct parameter {
  type_id type;
  ast::expr::expression* init;

  parameter(type_id t_type, ast::expr::expression* t_init = nullptr)
      : type(t_type),
        init(t_init) {}
};

using bound_argument = std::pair<parameter, binding_mode_t>;
using parameters     = memory::vector<parameter>;

struct callable {
  virtual ~callable()                               = default;
  virtual ast::identifier identifier() const        = 0;
  [[nodiscard]] virtual type_id return_type() const = 0;
  [[nodiscard]] cmm::parameters parameters() const {
    if (!m_parameters) {
      m_parameters = parameters_impl();
    }
    return m_parameters.value();
  }

protected:
  virtual cmm::parameters parameters_impl() const = 0;

private:
  mutable std::optional<cmm::parameters> m_parameters;
};

// template <typename T>
// concept Operand = (std::is_same_v<assembly::element*, T> || std::is_same_v<arg_t, T>);

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
  memory::vector<parameter> params;
  std::vector<instruction> ins;

  builtin_callable(operator_t op, decltype(ret) c, decltype(params) params, decltype(ins) e)
      : ret(c),
        params(std::move(params)),
        ins(std::move(e)) {}
  builtin_callable(std::string str, decltype(ret) c, decltype(params) params, decltype(ins) e)
      : ret(c),
        params(params),
        ins(e) {}

  ast::identifier identifier() const override;
  type_id return_type() const override { return ret; }
  [[nodiscard]] cmm::parameters parameters_impl() const override;
};

extern value_category_t get_value_category(types::type_id);
using value_category_conversor_type = std::function<value_category_t(value_category_t)>;

struct conversor : displayable {
  using condition_type = std::function<bool(value_category_t, type_id)>;

  conversor(std::string d,
            condition_type t_condition,
            types::modifier t_type_modifier,
            value_category_conversor_type t_value_mod)
      : m_desc(std::move(d)),
        m_condition(std::move(t_condition)),
        m_type_modifier(std::move(t_type_modifier)),
        m_value_category_modifier(std::move(t_value_mod)) {}

  [[nodiscard]] bool is_convertible(value_category_t t_cat, types::type_id t_type) const {
    return m_condition(t_cat, t_type);
  };
  [[nodiscard]] type_id convert_type(type_id t_type) const { return m_type_modifier(t_type); };
  [[nodiscard]] value_category_t convert_value_category(value_category_t t_cat) const {
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

extern const conversor lvalue_to_rvalue;
extern const conversor any_to_bool;

inline const std::array standard_conversions = {&any_to_bool, &lvalue_to_rvalue};

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
  // cmm::value* value;
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
