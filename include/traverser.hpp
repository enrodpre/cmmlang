#pragma once

#include <cstdint>
#include <string>
#include <sys/types.h>

#include "asm.hpp"
#include "ast.hpp"
#include "ast_visitor.hpp"
#include "lang.hpp"
#include "macros.hpp"

namespace cmm {
namespace ast {
namespace expr {
struct binary_operator;
struct call;
struct expression;
struct identifier;
struct implicit_conversion;
struct literal;
struct unary_operator;
} // namespace expr
} // namespace ast
} // namespace cmm

namespace cmm::ir {

using namespace ast;

struct compilation_unit;

enum class intent_t : uint8_t {
  MOVE_CONTENT,
  LOAD_VARIABLE_VALUE,
  LOAD_VARIABLE_ADDRESS,
};

struct expression_visitor;
struct statement_visitor;
struct global_visitor;

class ast_traverser {
public:
  ast_traverser() = delete;
  ast_traverser(compilation_unit&);
  NOT_MOVABLE_CLS(ast_traverser)
  NOT_COPYABLE_CLS(ast_traverser)

  void generate_program(translation_unit&);
  void generate_statements(decl::block&);
  void generate_statement(ast::statement*);
  assembly::operand* generate_expr(ast::expr::expression&,
                                   assembly::operand* = nullptr,
                                   intent_t           = intent_t::LOAD_VARIABLE_ADDRESS);

private:
  compilation_unit& m_context;
  translation_unit* ast;

  // Helpers
  template <bool IsGlobal>
  void generate_variable_decl(decl::variable*);
  template <typename Jump>
  void generate_continue_break(const Jump&);
  void generate_condition(expr::expression&, const std::string&);
  void begin_scope(decl::block&);
  void end_scope();

  friend statement_visitor;
  friend expression_visitor;
  friend global_visitor;
};

struct global_visitor : public visitor<GLOBAL_TYPES> {
  ast_traverser* gen;
  global_visitor(ast_traverser*);
  void visit(ast::decl::variable&) override;
  void visit(ast::decl::function&) override;
};

struct expression_visitor : public visitor<EXPRESSION_TYPES> {
  ast_traverser* gen;
  assembly::operand* in;
  assembly::operand* out;
  intent_t intent;
  expression_visitor(ast_traverser*, assembly::operand*, intent_t);

  void visit(ast::expr::call&) override;
  void visit(ast::expr::binary_operator&) override;
  void visit(ast::expr::unary_operator&) override;
  void visit(ast::expr::identifier&) override;
  void visit(ast::expr::literal&) override;
};

struct statement_visitor : public visitor<expr::expression, STATEMENT_TYPES, GLOBAL_TYPES> {
  ast_traverser* gen;
  statement_visitor(ast_traverser*);

  void visit(expr::expression&) override;
  void visit(ast::decl::block&) override;
  void visit(ast::decl::variable&) override;
  void visit(ast::decl::label&) override;
  void visit(ast::decl::function&) override;
  void visit(ast::iteration::for_&) override;
  void visit(ast::decl::function::definition&) override;
  void visit(ast::iteration::while_&) override;
  void visit(ast::selection::if_&) override;
  void visit(ast::jump::break_&) override;
  void visit(ast::jump::continue_&) override;
  void visit(ast::jump::goto_&) override;
  void visit(ast::jump::return_&) override;
};
} // namespace cmm::ir
