#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>

#include "asm.hpp"
#include "ast/tree.hpp"
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

using assembly::element;

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

  assembly::registers& regs;
  cmm::assembly::asmgen asmgen;
  translation_unit* ast;
  source_code* code;
  ast_traverser runner;

  ///////// FUNCTIONS //////////

  compilation_unit(ast::translation_unit*, source_code*);

  source_code* compile();

  [[nodiscard]] decl::function::definition* active_frame() noexcept { return stackframe.top(); }
  [[nodiscard]] const decl::function::definition* active_frame() const noexcept {
    return stackframe.top();
  }

  template <typename T, typename... Args>
    requires(std::is_base_of_v<assembly::element, T>)
  T* get_operand(Args&&...);

  void reserve_static_var(std::string_view);
  void reserve_memory(std::string_view, std::string_view, std::string_view);
  assembly::label_memory* reserve_constant(std::string_view);
  assembly::reg* call_builtin_operator(const operator_&, const ast::expr::arguments&);
  std::optional<assembly::reg*> call_function(const identifier&, const ast::expr::arguments&);

  template <typename... Args>
  void instruction(const instruction_t&, Args&&...);
  assembly::operand* move(assembly::operand*, assembly::operand*);
  void move_rsp(int64_t);
  assembly::reg* lea(assembly::reg*, assembly::operand*);
  assembly::reg* move_immediate(assembly::reg*, std::string_view);
  assembly::reg* return_reg(assembly::reg*);
  void push(assembly::reg*);
  void pop(assembly::reg*);
  void zero(assembly::reg*);
  void jump(std::string_view);
  void jump(const instruction_t&, std::string_view);
  void cmp(std::string_view, std::string_view);
  void exit(assembly::reg*);
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
