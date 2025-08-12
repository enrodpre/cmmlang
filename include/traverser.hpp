#pragma once

#include "asm.hpp"
#include "ast.hpp"
#include "ir.hpp"
#include "visitor.hpp"
#include <cstdint>

namespace cmm::ir {

struct compilation_unit;
namespace intents {
  enum class intent_t : uint8_t;
}

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
  void generate_statements(const ast::scope::block&);
  void generate_statement(const ast::statement*);
  assembly::operand* generate_expr(const ast::expr::expression&,
                                   ir::intents::intent_t,
                                   assembly::operand*);
  assembly::operand* generate_expr(const ast::expr::expression&, ir::intents::intent_t);
  assembly::operand* generate_expr(const ast::expr::expression&);
  assembly::operand* generate_expr(const ast::expr::expression&, cr_type, assembly::operand*);

private:
  compilation_unit& m_context;

  // Helpers
  template <bool IsGlobal>
  void generate_variable_decl(const ast::decl::variable*);
  template <typename Jump>
  void generate_continue_break(const Jump&);
  instruction_t generate_condition(const ast::expr::expression&);
  void begin_scope(const ast::scope::block*);
  void end_scope();
  std::optional<assembly::operand*> call_function(const ast::term::term&,
                                                  const std::vector<ptr_type>&,
                                                  const ast::expr::call::arguments&);
  std::optional<assembly::operand*> call_function(const ast::term::term&,
                                                  const ast::expr::call::arguments&);
  std::optional<assembly::operand*> call_function(const function*,
                                                  const ast::expr::call::arguments&);
  [[nodiscard]] std::optional<std::tuple<std::string, std::string>> get_label_interation_scope()
      const;

  friend statement_visitor;
  friend expression_visitor;
  friend global_visitor;
};

struct global_visitor : public const_visitor<GLOBAL_TYPES> {
  ast_traverser* gen;
  global_visitor(ast_traverser*);
  void visit(const ast::decl::variable&) override;
  void visit(const ast::decl::function&) override;
};

struct expression_visitor : public const_visitor<EXPRESSION_TYPES> {
  ast_traverser* gen;
  assembly::operand* in;
  assembly::operand* out;
  ir::intents::intent_t intent;
  expression_visitor(ast_traverser*, assembly::operand*, ir::intents::intent_t);

  void visit(const ast::expr::call&) override;
  void visit(const ast::expr::binary_operator&) override;
  void visit(const ast::expr::unary_operator&) override;
  void visit(const ast::expr::float_lit& c) override;
  void visit(const ast::expr::int_lit& c) override;
  void visit(const ast::expr::string_lit& c) override;
  void visit(const ast::expr::char_lit& c) override;
  void visit(const ast::expr::false_lit& c) override;
  void visit(const ast::expr::true_lit& c) override;
  void visit(const ast::expr::identifier&) override;
};

struct statement_visitor : public expression_visitor,
                           virtual public const_visitor<STATEMENT_TYPES,
                                                        ast ::scope ::block,
                                                        // ast ::scope ::function,
                                                        // ast ::scope ::namespace_,
                                                        // ast ::scope ::file,
                                                        GLOBAL_TYPES> {
  ast_traverser* gen;
  statement_visitor(ast_traverser*);

  void visit(const ast::scope::block& scope) override;
  // void visit(const ast::scope::namespace_& scope) override;
  // void visit(const ast::scope::function& scope) override;
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
};
} // namespace cmm::ir

#include <traverser.hpp>
