#include "ast_visitor.hpp"
#include "ast.hpp"
#include "expr.h"

using namespace cmm;
// using namespace cmm::ast;

ast::translation_unit* cmm::ast::find_root(const node* t_node) {
  parent_retriever_visitor<translation_unit*> visitor{};
  t_node->accept(visitor);
  auto res = visitor.get_result();
  if (!res.first) {
    throw error(std::format("Could not find the translation_unit of {}", typeid(t_node).name()));
  }
  return res.second;
}

ast::scope& ast::get_scope(node* t_node) { return *find_parent<ast::scope>(t_node); }

void ast::ast_visitor::visit(ast::literal& c) { TRACE_VISITOR(c); }

void ast::ast_visitor::visit(ast::keyword& c) { TRACE_VISITOR(c); }

void ast::ast_visitor::visit(ast::identifier& c) { TRACE_VISITOR(c); }

void ast::ast_visitor::visit(ast::decl::specifiers& c) {
  TRACE_VISITOR(c);
  c.storage.accept(*this);
  c.linkage.accept(*this);
  c.type.accept(*this);
};

void ast::ast_visitor::visit(ast ::expr ::identifier& c) { TRACE_VISITOR(c); };

void ast::ast_visitor::visit(ast ::expr ::unary_operator& c) {
  TRACE_VISITOR(c);
  c.operator_.accept(*this);
  c.expr->accept(*this);
};

void ast::ast_visitor::visit(ast ::expr ::binary_operator& c) {
  TRACE_VISITOR(c);
  c.operator_.accept(*this);
  c.left->accept(*this);
  c.right->accept(*this);
};

void ast::ast_visitor::visit(ast ::expr ::call& c) {
  TRACE_VISITOR(c);
  c.ident.accept(*this);
  c.args.accept(*this);
};

void ast::ast_visitor::visit(ast ::decl ::variable& c) {
  TRACE_VISITOR(c);
  c.ident.accept(*this);
  c.specs.accept(*this);
  if (auto* init = c.init) {
    init->accept(*this);
  }
};

void ast::ast_visitor::visit(ast ::decl ::function& c) {
  TRACE_VISITOR(c);
  c.specs.accept(*this);
  c.ident.accept(*this);
  for (auto& param : c.params) {
    param.accept(*this);
  }
  if (auto* body = c.body) {
    body->accept(*this);
  }
};

void ast::ast_visitor::visit(ast ::decl ::label& c) {
  TRACE_VISITOR(c);
  c.ident.accept(*this);
};

void ast::ast_visitor::visit(ast ::iteration ::while_& c) {
  TRACE_VISITOR(c);
  c.condition->accept(*this);
  if (auto* body = c.body) {
    body->accept(*this);
  }
};

void ast::ast_visitor::visit(ast ::iteration ::for_& c) {
  TRACE_VISITOR(c);
  c.start->accept(*this);
  if (auto* cond = c.condition) {
    cond->accept(*this);
  }
  if (auto* step = c.step) {
    step->accept(*this);
  }
  if (auto* body = c.body) {
    body->accept(*this);
  }
};

void ast::ast_visitor::visit(ast ::selection ::if_& c) {
  TRACE_VISITOR(c);
  c.condition->accept(*this);
  if (auto* block = c.block) {
    block->accept(*this);
  }
  if (auto* else_ = c.else_) {
    else_->accept(*this);
  }
};

void ast::ast_visitor::visit(ast ::jump ::goto_& c) {
  TRACE_VISITOR(c);
  c.term.accept(*this);
};

void ast::ast_visitor::visit(ast ::jump ::return_& c) {
  TRACE_VISITOR(c);
  if (auto* expr = c.expr) {
    expr->accept(*this);
  }
}

void ast::ast_visitor::visit(ast::jump::continue_& c) { TRACE_VISITOR(c); }

void ast::ast_visitor::visit(ast::expr::literal& c) { TRACE_VISITOR(c); }

void ast::ast_visitor::visit(ast::jump::break_& c) { TRACE_VISITOR(c); }

void ast::ast_visitor::visit(ast::operator_& c) { TRACE_VISITOR(c); }

void ast::ast_visitor::visit(ast::storage_spec& c) { TRACE_VISITOR(c); }

void ast::ast_visitor::visit(ast::type_spec& c) { TRACE_VISITOR(c); }

void ast::ast_visitor::visit(ast::linkage_spec& c) { TRACE_VISITOR(c); }

void ast::ast_visitor::visit(expr::arguments& c) { TRACE_VISITOR(c); }

void ast::ast_visitor::visit(ast::decl::function::definition& c) {
  TRACE_VISITOR(c);
  for (const auto& stmt : c.stmts) {
    stmt->accept(*this);
  }
}

void ast::ast_visitor::visit(ast::decl::block& c) {
  TRACE_VISITOR(c);
  for (const auto& stmt : c.stmts) {
    stmt->accept(*this);
  }
}

void ast::const_ast_visitor::visit(const ast::operator_& c) { TRACE_VISITOR(c); }

void ast::const_ast_visitor::visit(const ast::literal& c) { TRACE_VISITOR(c); }

void ast::const_ast_visitor::visit(const ast::keyword& c) { TRACE_VISITOR(c); }

void ast::const_ast_visitor::visit(const ast::identifier& c) { TRACE_VISITOR(c); }

void ast::const_ast_visitor::visit(const expr::arguments& c) { TRACE_VISITOR(c); }

void ast::const_ast_visitor::visit(const ast::decl::specifiers& s) {
  TRACE_VISITOR(s);
  s.accept(*this);
};

void ast::const_ast_visitor::visit(const ast ::expr ::identifier& c) { TRACE_VISITOR(c); };

void ast::const_ast_visitor::visit(const ast ::expr ::unary_operator& c) {
  TRACE_VISITOR(c);
  c.operator_.accept(*this);
  c.expr->accept(*this);
};

void ast::const_ast_visitor::visit(const ast ::expr ::binary_operator& c) {
  TRACE_VISITOR(c);
  c.operator_.accept(*this);
  c.left->accept(*this);
  c.right->accept(*this);
};

void ast::const_ast_visitor::visit(const ast ::expr ::call& c) {
  TRACE_VISITOR(c);
  c.ident.accept(*this);
  c.args.accept(*this);
};

void ast::const_ast_visitor::visit(const ast ::decl ::variable& c) {
  TRACE_VISITOR(c);
  c.ident.accept(*this);
  c.specs.accept(*this);
  if (auto* init = c.init) {
    init->accept(*this);
  }
};

void ast::const_ast_visitor::visit(const ast ::decl ::function& c) {
  TRACE_VISITOR(c);
  c.specs.accept(*this);
  c.ident.accept(*this);
  for (const auto& param : c.params) {
    param.accept(*this);
  }
  if (auto* body = c.body) {
    body->accept(*this);
  }
};

void ast::const_ast_visitor::visit(const ast ::decl ::label& c) {
  TRACE_VISITOR(c);
  c.ident.accept(*this);
};

void ast::const_ast_visitor::visit(const ast ::iteration ::while_& c) {
  TRACE_VISITOR(c);
  c.condition->accept(*this);

  if (auto* body = c.body) {
    body->accept(*this);
  }
};

void ast::const_ast_visitor::visit(const ast ::iteration ::for_& c) {
  TRACE_VISITOR(c);
  c.start->accept(*this);
  if (auto* cond = c.condition) {
    cond->accept(*this);
  }
  if (auto* step = c.step) {
    step->accept(*this);
  }
  if (auto* body = c.body) {
    body->accept(*this);
  }
};

void ast::const_ast_visitor::visit(const ast ::selection ::if_& c) {
  TRACE_VISITOR(c);
  c.condition->accept(*this);
  if (auto* block = c.block) {
    block->accept(*this);
  }
  if (auto* else_ = c.else_) {
    else_->accept(*this);
  }
};

void ast::const_ast_visitor::visit(const ast ::jump ::goto_& c) {
  TRACE_VISITOR(c);
  c.term.accept(*this);
};
void ast::const_ast_visitor::visit(const ast ::jump ::return_& c) {
  TRACE_VISITOR(c);
  if (auto* expr = c.expr) {
    expr->accept(*this);
  }
}
void ast::const_ast_visitor::visit(const ast::jump::continue_& c) { TRACE_VISITOR(c); }
void ast::const_ast_visitor::visit(const ast::jump::break_& c) { TRACE_VISITOR(c); }
void ast::const_ast_visitor::visit(const ast::storage_spec& c) { TRACE_VISITOR(c); }
void ast::const_ast_visitor::visit(const ast::linkage_spec& c) { TRACE_VISITOR(c); }
void ast::const_ast_visitor::visit(const ast::type_spec& c) { TRACE_VISITOR(c); }
void ast::const_ast_visitor::visit(const ast::expr::literal& c) { TRACE_VISITOR(c); }
void ast::const_ast_visitor::visit(const ast::decl::function::definition& c) {
  TRACE_VISITOR(c);
  for (const auto& stmt : c.stmts) {
    stmt->accept(*this);
  }
}

void ast::const_ast_visitor::visit(const ast::decl::block& c) {
  TRACE_VISITOR(c);
  for (const auto& stmt : c.stmts) {
    stmt->accept(*this);
  }
}
