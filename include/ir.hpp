#pragma once

#include "asm.hpp"
#include "ast.hpp"
#include "common.hpp"
#include "expr.h"
#include "lang.hpp"
#include "traverser.hpp"
#include <cstdint>
#include <libassert/assert.hpp>
#include <optional>

namespace cmm::ir {

using assembly::operand;

namespace intents {
  enum class address_intent_t : uint8_t { HAVING_VALUE = 0, HAVING_ADDRESS, CARENT };
  enum class address_mode_intent_t : uint8_t { COPY = 0, ADDRESS_MEMORY, LOAD_ADDRESS };

  enum class intent_t : uint8_t {
    MOVE_CONTENT,
    LOAD_VARIABLE_VALUE,
    LOAD_VARIABLE_ADDRESS,
    SAVE_VARIABLE_VALUE
  };
} // namespace intents

enum class Phase : uint8_t {
  STOPPED = 0,
  GLOBAL,
  EXECUTING,
  PRUNING_FULL,  // Full prune of current branch
  PRUNING_LABEL, // Prune until last.pruning_branch label found
  EXITING
};

class compilation_unit : public default_singleton<compilation_unit> {
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

  void reserve_static_var(cstring);
  void reserve_memory(cstring, cstring, cstring);
  assembly::label_literal* reserve_constant(cstring);
  std::optional<operand*> call_function(decl::signature, const ast::expr::arguments&, bool = false);

  template <assembly::Operand... Args>
  void instruction(const instruction_t&, Args&&...);
  operand* builtin_operator(expr::unary_operator&, operand*);
  operand* builtin_operator(expr::binary_operator&, operand*, operand*);
  operand* move(operand*, operand*);
  operand* lea(operand*, operand*);
  operand* move_immediate(operand*, cstring);
  operand* return_reg(operand*);
  void push(operand*);
  void pop(operand*);
  operand* zero(operand*);
  void jump(cstring);
  void jump(const instruction_t&, cstring);
  // void move_rsp(size_t);
  void cmp(cstring, cstring);
  void call(cstring);
  void exit(operand*);
  void exit_successfully();
  void syscall();
  void syscall(cstring);
  void ret();
  void label(cstring);
  void comment(cstring);

  friend default_singleton<compilation_unit>;

protected:
  compilation_unit();

private:
  void start();
  std::string end();
};

} // namespace cmm::ir
#include "ir.inl"
