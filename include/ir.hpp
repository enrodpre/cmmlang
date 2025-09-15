#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

#include "asm.hpp"
#include "ast.hpp"
#include "common.hpp"
#include "compiler.hpp"
#include "lang.hpp"
#include "traverser.hpp"

namespace cmm {
class source_code;

namespace ast {
struct identifier;
struct translation_unit;
} // namespace ast
} // namespace cmm

namespace cmm::ir {

using assembly::operand;

enum class Phase : uint8_t {
  STOPPED = 0,
  GENERATING,
  PRUNING_FULL,  // Full prune of current branch
  PRUNING_LABEL, // Prune until last.pruning_branch label found
  EXITING
};

struct compilation_unit {
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
  Phase current_phase = Phase::STOPPED;

  assembly::registers regs;
  cmm::assembly::asmgen asmgen;
  translation_unit* ast;
  const source_code* code;
  ast_traverser runner;

  ///////// FUNCTIONS //////////

  compilation_unit(ast::translation_unit*, const source_code*);

  std::string compile();

  [[nodiscard]] decl::function::definition* active_frame() noexcept { return stackframe.top(); }
  [[nodiscard]] const decl::function::definition* active_frame() const noexcept {
    return stackframe.top();
  }
  void reserve_static_var(std::string_view);
  void reserve_memory(std::string_view, std::string_view, std::string_view);
  assembly::label_literal* reserve_constant(std::string_view);
  operand* call_builtin_operator(const operator_&, const ast::expr::arguments&);
  std::optional<operand*> call_function(const identifier&, const ast::expr::arguments&);

  template <assembly::Operand... Args>
  void instruction(const instruction_t&, Args&&...);
  operand* move(operand*, operand*);
  void move_rsp(int64_t);
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

  stack<decl::function::definition*> stackframe;

private:
  void start();
  std::string end();

  void load_arguments(const parameters&, const expr::arguments&);
};

} // namespace cmm::ir

#include "ir.inl"
