#pragma once

#include "asm.hpp"
#include "ast.hpp"
#include <cstdint>

namespace cmm::ir {

struct compilation_unit;
namespace intents {
  enum class intent_t : uint8_t;
}

using assembly::operand;

// using callback_body_t = std::function<void(compilation_unit&)>;
// enum class callback_t : uint8_t { EXEC_INSTRUCTION = 0, SAVE_VARIABLE };

// using callback_loader_t = std::function<callback_body_t(ir::variable, reg*)>;

// struct callbacks {
//   STATIC_CLS(callbacks)
//   static const callback_loader_t EXEC_INSTRUCTION;
//   static const callback_loader_t SAVE_VARIABLE;
// };

struct expression_visitor;
struct statement_visitor;
struct global_visitor;

class ast_traverser {
public:
  ast_traverser(compilation_unit&);
  ast_traverser() = delete;
  NOT_MOVABLE_CLS(ast_traverser)
  NOT_COPYABLE_CLS(ast_traverser)

  void generate_program(const ast::program&);
  void generate_statements(const ast::compound&);
  void generate_statement(const ast::statement&);
  operand* generate_expr(const ast::expr::expression&,
                         ir::intents::intent_t,
                         operand*);
  operand* generate_expr(const ast::expr::expression&, ir::intents::intent_t);
  operand* generate_expr(const ast::expr::expression&);

private:
  compilation_unit& m_context;
  // std::queue<callback_body_t> m_callbacks;

  // Helpers
  template <bool IsGlobal>
  void generate_variable_decl(const ast::decl::variable&);
  template <typename Jump>
  void generate_continue_break(const Jump&);
  instruction_t generate_condition(const ast::expr::expression&);
  void begin_scope(const ast::compound*);
  void end_scope();
  operand* call_function(const ast::term::identifier&,
                         ast::expr::call::arguments);
  operand* call_operator(const ast::term::operator_&, ast::expr::expression&);
  operand* call_operator(const ast::term::operator_&,
                         ast::expr::expression&,
                         ast::expr::expression&);
  [[nodiscard]] std::optional<std::tuple<std::string, std::string>>
  get_label_interation_scope() const;

  friend statement_visitor;
  friend expression_visitor;
  friend global_visitor;
};

struct global_visitor : public revisited::Visitor<GLOBAL_TYPES> {
  ast_traverser* gen;
  global_visitor(ast_traverser*);
  void visit(const ast::decl::variable&) override;
  void visit(const ast::decl::function&) override;
};
struct expression_visitor : public revisited::Visitor<EXPRESSION_TYPES> {
  ast_traverser* gen;
  operand* in;
  operand* out;
  ir::intents::intent_t intent;
  expression_visitor(ast_traverser*, operand*, ir::intents::intent_t);

  void visit(const ast::expr::call&) override;
  void visit(const ast::expr::binary_operator&) override;
  void visit(const ast::expr::unary_operator&) override;
  void visit(const ast::expr::literal&) override;
  void visit(const ast::expr::identifier&) override;
};

struct statement_visitor : public revisited::Visitor<STATEMENT_TYPES> {
  ast_traverser* gen;
  statement_visitor(ast_traverser*);

  void visit(const ast::compound& scope) override;
  void visit(const ast::decl::variable& vardecl) override;
  void visit(const ast::decl::label& label_) override;
  void visit(const ast::decl::function& func) override;
  void visit(const ast::iteration::for_& for_) override;
  void visit(const ast::iteration::while_& while_) override;
  void visit(const ast::selection::if_& if_) override;
  void visit(const ast::jump::break_& break_) override;
  void visit(const ast::jump::continue_& continue_) override;
  void visit(const ast::jump::goto_& goto_) override;
  void visit(const ast::jump::return_& ret) override;
  void visit(const ast::expr::expression& expr) override;
};
} // namespace cmm::ir

#include <traverser.hpp>
