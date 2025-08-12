#pragma once

#include "allocator.hpp"
#include "asm.hpp"
#include "ast.hpp"
#include "common.hpp"
#include "lang.hpp"
#include "semantic.hpp"
#include "traverser.hpp"
#include "types.hpp"
#include <cstdint>
#include <libassert/assert.hpp>
#include <optional>
#include <type_traits>

namespace cmm::ir {

using assembly::operand;

constexpr static uint8_t DATASIZE = 8;

template <typename T, typename... Ts>
concept IsAnyType = (std::is_same_v<T, Ts> || ...);

namespace intents {
  enum class address_intent_t : uint8_t { HAVING_VALUE = 0, HAVING_ADDRESS, CARENT };
  enum class address_mode_intent_t : uint8_t { COPY = 0, ADDRESS_MEMORY, LOAD_ADDRESS };

  template <auto... Ts>
  struct intent {};

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

namespace ast {
  class program;
}

class compilation_unit : public default_singleton<compilation_unit> {
public:
  ast_traverser runner;
  std::string compile(ast::program&, const source_code*);

  /////////// OBJECTS //////////

  struct {
    std::optional<operator_t> operator_                = std::nullopt;
    const ::cmm::ast::terms::identifier* pruning_label = nullptr;
  } last;
  struct {
    size_t whiles   = 0;
    size_t fors     = 0;
    size_t literals = 0;
  } counters;
  std::optional<instruction_t> next_jump;
  size_t next_offset  = 0;
  Phase current_phase = Phase::STOPPED;
  location src_location;
  const cmm::ast::statement* current_statement{};
  const source_code* source = nullptr;
  assembly::registers regs;

  cmm::assembly::asmgen asmgen;

  //////////// OBJECTS ///////////
  [[nodiscard]] std::string current_line() const;

  const variable* declare_local_variable(const ::cmm::ast::decl::variable&, operand*);
  const variable* declare_global_variable(const variable&, operand*);
  void declare_label(const ::cmm::ast::decl::label&);
  [[nodiscard]] operand* get_variable_address(const ast::terms::identifier&) const;
  void save_variable(const variable*, operand*);
  void reserve_memory(cstring, cstring, cstring);
  assembly::label_literal* reserve_constant(cstring);
  std::optional<operand*> call_builtin(const builtin_signature_t&, const std::vector<operand*>&);
  void create_frame(const cmm::ast::scopes::block&);

  template <typename... Args>
  void instruction(const instruction_t&, Args&&...);
  operand* move(operand*, operand*);
  operand* lea(operand*, operand*);
  operand* move_immediate(operand*, cstring);
  operand* return_reg(operand*);
  void push(operand*);
  void pop(operand*);
  operand* zero(operand*);
  void jump(cstring);
  void jump(const instruction_t&, cstring);
  void move_rsp(size_t);
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

namespace builtin {}; // namespace cmm::ir
//
#include "ir.inl"
