#pragma once

#include "compiler.hpp"
#include <cstddef>
#include <cstdint>

#include <optional>
#include <string>

#include "asm.hpp"
#include "ast.hpp"
#include "common.hpp"
#include "lang.hpp"
#include "traverser.hpp"

namespace cmm {
namespace ast {
struct identifier;
struct translation_unit;
} // namespace ast
} // namespace cmm

namespace cmm::ir {

using assembly::operand;

enum class address_intent_t : uint8_t { HAVING_VALUE = 0, HAVING_ADDRESS, CARENT };
enum class address_mode_intent_t : uint8_t { COPY = 0, ADDRESS_MEMORY, LOAD_ADDRESS };

enum class Phase : uint8_t {
  STOPPED = 0,
  GLOBAL,
  EXECUTING,
  PRUNING_FULL,  // Full prune of current branch
  PRUNING_LABEL, // Prune until last.pruning_branch label found
  EXITING
};

struct compilation_unit : public default_singleton<compilation_unit> {
public:
  /////////// OBJECTS //////////

  struct {
    std::optional<operator_t> operator_         = std::nullopt;
    const ::cmm::ast::identifier* pruning_label = nullptr;
  } last;

  struct {
    size_t whiles   = 0;
    size_t fors     = 0;
    size_t literals = 0;
  } counters;

  std::optional<instruction_t> next_jump;
  Phase current_phase       = Phase::STOPPED;
  const source_code* source = nullptr;
  assembly::registers regs;

  cmm::assembly::asmgen asmgen;
  translation_unit* ast;
  ast_traverser runner;

  ///////// FUNCTIONS //////////

  [[nodiscard]] std::string current_line() const;

  std::string compile(translation_unit&, const source_code*);

  void reserve_static_var(std::string_view);
  void reserve_memory(std::string_view, std::string_view, std::string_view);
  assembly::label_literal* reserve_constant(std::string_view);
  std::optional<operand*> call_function(const identifier& id,
                                        const ast::expr::arguments&,
                                        bool = false);
  template <typename T, typename Id = T::identifier_t>
  const T* get_callable(Id id, const expr::arguments&) const;
  operand* call_builtin_operator(const operator_&, const ast::expr::arguments&);

  template <assembly::Operand... Args>
  void instruction(const instruction_t&, Args&&...);
  operand* move(operand*, operand*);
  operand* lea(operand*, operand*);
  operand* move_immediate(operand*, std::string_view);
  operand* return_reg(operand*);
  void push(operand*);
  void pop(operand*);
  operand* zero(operand*);
  void jump(std::string_view);
  void jump(const instruction_t&, std::string_view);
  void cmp(std::string_view, std::string_view);
  void exit(operand*);
  void call(std::string_view);
  void exit_successfully();
  void syscall();
  void syscall(std::string_view);
  void ret();
  void label(std::string_view);
  void comment(std::string_view);

  friend default_singleton<compilation_unit>;

protected:
  compilation_unit();

private:
  void start();
  std::string end();

  [[nodiscard]] std::vector<bound_argument> bind_parameters(const std::vector<parameter>&,
                                                            const expr::arguments&) const;
  std::vector<operand*> load_arguments(const std::vector<bound_argument>&);

  template <typename T, typename Id = typename T::identifier_t>
  const T* resolve_overloads(Id, std::vector<const T*>, const expr::arguments&) const;
  [[nodiscard]] static bool match_arguments(const std::vector<types::type_id>&,
                                            const std::vector<types::type_id>&);
};

} // namespace cmm::ir

#include "ir.inl"
