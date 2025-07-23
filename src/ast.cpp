#include "ast.hpp"
#include "common.hpp"
#include <utility>

namespace cmm::ast {

FORMAT_IMPL(term::keyword, "{}", kind);
FORMAT_IMPL(term::literal, "{}", value);
FORMAT_IMPL(term::operator_, "{}", type);
FORMAT_IMPL(term::identifier, "{}", value);
FORMAT_IMPL(term::specifier, "{}", type);
FORMAT_IMPL(expr::identifier, "{}", term.value);
FORMAT_IMPL(expr::literal, "{}", term.value);
FORMAT_IMPL(expr::call, "{}()", ident); // args.join(", "));
FORMAT_IMPL(expr::unary_operator,
            "Unary operator_t:\n  {}\n  {}",
            operator_,
            ""); // expr);
FORMAT_IMPL(expr::binary_operator,
            "Binary operator_t:\n  {}\n  {}\n  {}",
            "", // left,
            operator_,
            ""); // right);
/* FORMAT_IMPL(compound, "Compound({}):\n  {}", size(), *this); */
FORMAT_IMPL(decl::label, "LabelDecl({})", term);
FORMAT_IMPL(decl::variable, "VarDecl:  {} {}", *ident, *init);
FORMAT_IMPL(decl::function, "{}", "Function decl: ");
FORMAT_IMPL(iteration::while_, "While:\n  {}", condition);
FORMAT_IMPL(iteration::for_, "For:\n  {}", *condition);
FORMAT_IMPL(selection::if_, "If:\n  {}\n  {}", condition, *block);
FORMAT_IMPL(jump::break_, "{}", "Break");
FORMAT_IMPL(jump::continue_, "{}", "Continue");
FORMAT_IMPL(jump::return_, "{}: {}", "Return: ", *expr);
FORMAT_IMPL(jump::goto_, "Goto({})", term);
// FORMAT_IMPL(statement, "Goto()", nullptr);

expr::identifier::identifier(ast::term::identifier&& id)
    : DerivedVisitable(id.loc),
      term(std::move(id)) {}
expr::literal::literal(const token& token)
    : DerivedVisitable(token.location),
      term(token),
      type(token.type.get_properties().type.value()) {}
[[nodiscard]] std::string expr::literal::to_asm() const {
  switch (type) {
    case type_t::bool_t:
      if (term.value == "false") {
        return "1";
      } else {
        return "0";
      }
      break;
    case type_t::void_t:
    case type_t::nullptr_t:
    case type_t::char_t:
    case type_t::byte_t:
    case type_t::int_t:
    case type_t::short_t:
    case type_t::double_t:
    case type_t::long_t:
    case type_t::float_t:
    case type_t::class_t:
    case type_t::enum_t:
    case type_t::struct_t:
    default:
      return term.value;
      break;
  }
}
expr::call::call(decltype(ident)&& ident_, std::optional<decltype(args)> args)
    : DerivedVisitable(ident_.loc, args.has_value() ? args->loc : location()),
      ident(ident_),
      args(std::move(*args)) {
  // ident.set_parent(this);
  args->set_parent(this);
}
expr::unary_operator::unary_operator(expression& expression,
                                     term::operator_&& op)
    : DerivedVisitable(expression.loc, op.loc),
      expr(expression),
      operator_(std::move(op)) {
  expr.set_parent(this);
  operator_.set_parent(this);
}
using namespace decl;

label::label(const token& label_)
    : DerivedVisitable(label_.location),
      term(label_) {}

variable::variable(struct specifiers&& mods,
                   decltype(ident) id,
                   decltype(init) i)
    : DerivedVisitable(mods.loc, GET_LOC(id), GET_LOC(i)),
      specifiers(std::move(mods)),
      ident(id),
      init(i) {
  specifiers.set_parent(this);
  if (ident != nullptr) {
    ident->parent = this;
  }
  if (init != nullptr) {
    init->set_parent(this);
  }
}
function::function(struct specifiers mods,
                   decltype(ident) ident_,
                   decltype(parameters) args,
                   compound* body_)
    : DerivedVisitable(specifiers.loc,
                       ident_.loc,
                       parameters.loc,
                       GET_LOC(body_)),
      specifiers(std::move(mods)),
      ident(ident_),
      parameters(std::move(args)),
      body(body_) {}

expr::binary_operator::binary_operator(expression& left,
                                       expression& right,
                                       term::operator_&& op)
    : DerivedVisitable(left.loc, right.loc, op.loc),
      left(left),
      right(right),
      operator_(std::move(op)) {
  left.set_parent(this);
  right.set_parent(this);
  operator_.set_parent(this);
}
selection::if_::if_(term::keyword k,
                    decltype(condition) condition,
                    decltype(block) block,
                    decltype(else_) else_)
    : DerivedVisitable(k.loc, condition.loc, block->loc, GET_LOC(else_)),
      keyword(std::move(k)),
      condition(condition),
      block(block),
      else_(else_) {
  keyword.set_parent(this);
  condition.set_parent(this);
  if (else_ != nullptr) {
    else_->set_parent(this);
  }
  if (else_ != nullptr) {
    else_->set_parent(this);
  }
}

template <typename It>
std::string iteration::iteration<It>::condition_label() const {
  return std::format("cond_{}", static_cast<const It*>(this)->repr());
}
template <typename It>
std::string iteration::iteration<It>::exit_label() const {
  return std::format("exit_{}", static_cast<const It*>(this)->repr());
}

iteration::while_::while_(term::keyword k,
                          expr::expression& condition_,
                          statement* block)
    : DerivedVisitable(k.loc, condition_.loc, GET_LOC(block)),
      keyword(k),
      condition(condition_),
      body(block) {
  keyword.set_parent(this);
  condition.set_parent(this);
  if (body != nullptr) {
    body->set_parent(this);
  }
}

iteration::for_::for_(term::keyword k,
                      decl::variable* start_,
                      expr::expression* condition_,
                      expr::expression* step_,
                      statement* block)
    : DerivedVisitable(k.loc,
                       GET_LOC(start_),
                       GET_LOC(condition_),
                       GET_LOC(step_),
                       GET_LOC(block)),
      keyword(k),
      start(start_),
      condition(condition_),
      step(step_),
      body(block) {
  keyword.set_parent(this);
  if (condition != nullptr) {
    condition->set_parent(this);
  }
  if (body != nullptr) {
    body->set_parent(this);
  }
  if (start != nullptr) {
    start->set_parent(this);
  }
  if (block != nullptr) {
    step->set_parent(this);
  }
}

jump::goto_::goto_(const token& token)
    : DerivedVisitable(token),
      term(token) {
  term.set_parent(this);
}
jump::break_::break_(const token& token)
    : DerivedVisitable(token),
      keyword(token) {
  keyword.set_parent(this);
}
jump::continue_::continue_(const token& token)
    : DerivedVisitable(token),
      keyword(token) {
  keyword.set_parent(this);
}
jump::return_::return_(term::keyword k, expr::expression* expr_)
    : DerivedVisitable(k.loc + GET_LOC(expr_)),
      keyword(std::move(k)),
      expr(expr_) {
  keyword.set_parent(this);
}

} // namespace cmm::ast
