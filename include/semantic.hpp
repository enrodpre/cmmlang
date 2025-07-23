#pragma once

#include "ast.hpp"
#include <type_traits>

#define SET_PARENT_AND_VISIT(node, member) \
  node.member.set_parent(&(node)); \
  std::visit(this, (node).member);

#define RANGE_SET_PARENT_AND_VISIT(node, range) \
  for (auto& elem : node.range) { \
    elem.set_parent(&(node)); \
    std::visit(this, elem); \
  }

namespace cmm::ast {
class semantics {
  // struct semantics_visitor : base_visitor {
  //   void operator()(ast::compound& c) override { c.accept(*this); }
  //   /* void operator()(ast::expr::identifier& c) override
  //   {c.load_semantics(kj)}
  //    */
  //   void visit(ast::expr::literal& c) override {}
  //   void visit(ast::expr::unary_operator& c) override {}
  //   void visit(ast::expr::binary_operator& c) override {}
  //   void visit(ast::expr::call& c) override {}
  //   void visit(ast::decl::variable& c) override {}
  //   void visit(ast::decl::function& c) override {}
  //   void visit(ast::decl::label& c) override {}
  //   void visit(ast::iteration::while_& c) override {}
  //   void visit(ast::iteration::for_& c) override {}
  //   void visit(ast::selection::if_& c) override {}
  //   void visit(ast::jump::goto_& c) override {}
  //   void visit(ast::jump::return_& c) override {}
  //   void visit(ast::statement& c) override {}
  // };
  //
public:
  /* template <typename T> */
  /* static void set_parent_nodes(program& program_) */
  /* { */
  /*   struct parent_visitor : public visitor { */
  /*     auto create_cb(node& parent) */
  /*     { */
  /*       return [&parent](node& node_) { node_.set_parent(&parent); }; */
  /*     } */
  /**/
  /*     void operator()(compound& compound_) override */
  /*     { */
  /*       for (auto& elem : compound_) { */
  /*         std::visit([&compound_](auto&& obj) { obj->set_parent(compound_);
   * }, *elem);
   */
  /*       } */
  /*     } */
  /*     void operator()(term::identifier& identifier_) override {} */
  /*     void operator()(term::keyword& keyword_) override {} */
  /*     void operator()(term::literal& literal_) override {} */
  /*     void operator()(term::specifier& specifier_) override {} */
  /*     void operator()(term::operator_& operator_) override {} */
  /*     void operator()(expr::identifier& identifier_) override */
  /*     { */
  /*       SET_PARENT_AND_VISIT(identifier_, ident); */
  /*     } */
  /*     void operator()(expr::literal& literal_) override */
  /*     { */
  /*       literal_.lit.set_parent(&literal_); */
  /*     } */
  /*     void operator()(expr::binary_operator& binary_operator_) override */
  /*     { */
  /*       binary_operator_.operator_.set_parent(&binary_operator_); */
  /*       binary_operator_.accept(); */
  /*       VARIANT_SET_PARENT_AND_VISIT(binary_operator_, right); */
  /*     } */
  /*     void operator()(expr::unary_operator& unary_operator_) override */
  /*     { */
  /*       SET_PARENT_AND_VISIT(unary_operator_, operator_); */
  /*       VARIANT_SET_PARENT_AND_VISIT(unary_operator_, expr); */
  /*     } */
  /*     void operator()(expr::call& call_) override */
  /*     { */
  /*       SET_PARENT_AND_VISIT(call_, ident); */
  /*       RANGE_VARIANT_SET_PARENT_AND_VISIT(call_, args); */
  /*     } */
  /*     void operator()(decl::label& label_) override */
  /*     { */
  /*       SET_PARENT_AND_VISIT(label_, ident); */
  /*     } */
  /*     void operator()(decl::variable& variable_) override */
  /*     { */
  /*       RANGE_SET_PARENT_AND_VISIT(variable_, specs); */
  /*       SET_PARENT_AND_VISIT(variable_, ident); */
  /*       VARIANT_SET_PARENT_AND_VISIT(variable_, init); */
  /*     } */
  /*     void operator()(decl::function& function_) override */
  /*     { */
  /*       RANGE_SET_PARENT_AND_VISIT(function_, specs); */
  /*       SET_PARENT_AND_VISIT(function_, ident); */
  /*       RANGE_SET_PARENT_AND_VISIT(function_, args); */
  /*       SET_PARENT_AND_VISIT(function_, body); */
  /*     } */
  /*     void operator()(iteration::while_& while_) override */
  /*     { */
  /*       SET_PARENT_AND_VISIT(while_, keyword); */
  /*       VARIANT_SET_PARENT_AND_VISIT(while_, condition); */
  /*       VARIANT_SET_PARENT_AND_VISIT(while_, body) */
  /*     } */
  /*     void operator()(iteration::for_& for_) override */
  /*     { */
  /*       SET_PARENT_AND_VISIT(for_, keyword); */
  /*       for_.start->set_parent(&for_); */
  /*       operator()(for_); */
  /*       VARIANT_SET_PARENT_AND_VISIT(for_, step) */
  /*       VARIANT_SET_PARENT_AND_VISIT(for_, condition) */
  /*       VARIANT_SET_PARENT_AND_VISIT(for_, body) */
  /*     } */
  /*     void operator()(selection::if_& if_) override */
  /*     { */
  /*       SET_PARENT_AND_VISIT(if_, keyword); */
  /*       VARIANT_SET_PARENT_AND_VISIT(if_, condition) */
  /*       VARIANT_SET_PARENT_AND_VISIT(if_, block) */
  /*       VARIANT_SET_PARENT_AND_VISIT(if_, else_) */
  /*     } */
  /*     void operator()(jump::goto_& goto_) override */
  /*     { */
  /*       SET_PARENT_AND_VISIT(goto_, keyword); */
  /*     } */
  /*     void operator()(jump::break_& break_) override {} */
  /*     void operator()(jump::continue_& continue_) override {} */
  /*     void operator()(jump::return_& return_) override */
  /*     { */
  /*       SET_PARENT_AND_VISIT(return_, keyword); */
  /*       SET_PARENT_AND_VISIT(return_, expr); */
  /*     } */
  /*     void operator()(expr::expression*) override {} */
  /*     void operator()(statement*) override {} */
  /*   }; */
  /**/
  /*   parent_visitor visitor_; */
  /*   program_.accept(visitor_); */
  /* }; */
};
}; // namespace cmm::ast
