#include "ast.hpp"
#include "common.hpp"
#include <utility>

namespace cmm::ast {

namespace {
  template <typename>
  inline constexpr bool always_false = false;
  auto extract_location() { return location(); }
  template <typename T>
  auto extract_location(T&& t) {
    if constexpr (Allocated<T>) {
      return t.location();
    } else if constexpr (AllocatedPtr<T>) {
      return t == nullptr ? location() : t->location();
    } else if constexpr (std::is_same_v<location, std::remove_cvref_t<T>>) {
      return t;
    } else {
      return location();
    }
  }
  template <typename... Ts>
  constexpr location sum_locations(Ts&&... ts) {
    return (extract_location(std::forward<Ts>(ts)) + ...);
  }
} // namespace

static_assert(AllocatedPtr<const term::identifier*>);

FORMAT_IMPL(term::keyword, "{}", kind);
FORMAT_IMPL(term::literal, "{}", value);
FORMAT_IMPL(term::operator_, "{}", type);
FORMAT_IMPL(term::identifier, "{}", value);
FORMAT_IMPL(term::specifier, "{}", type);
FORMAT_IMPL(expr::identifier, "{}", term.value);
FORMAT_IMPL(expr::literal, "{}", term.value);
FORMAT_IMPL(expr::call, "{}()", ident); // args.join(", "));
FORMAT_IMPL(expr::unary_operator, "Unary operator_t:\n  {}\n  {}", operator_,
            ""); // expr);
FORMAT_IMPL(expr::binary_operator,
            "Binary operator_t:\n  {}\n  {}\n  {}",
            "", // left,
            operator_,
            ""); // right);
/* FORMAT_IMPL(compound, "Compound({}):\n  {}", size(), *this); */
FORMAT_IMPL(decl::label, "LabelDecl({})", term);
FORMAT_IMPL(decl::variable, "VarDecl: DerivedVisitable {} {}", *ident, *init);
FORMAT_IMPL(decl::function, "Function decl: {}", ident.value);
FORMAT_IMPL(iteration::while_, "While:\n  {}", condition);
FORMAT_IMPL(iteration::for_, "For:\n  {}", *condition);
FORMAT_IMPL(selection::if_, "If:\n  {}\n  {}", condition, *block);
FORMAT_IMPL(jump::break_, "{}", "Break");
FORMAT_IMPL(jump::continue_, "{}", "Continue");
FORMAT_IMPL(jump::return_, "{}: DerivedVisitable{}", "Return: ", *expr);
FORMAT_IMPL(jump::goto_, "Goto({})", term);

compound::compound(std::vector<statement*>&& v)
    : siblings(std::move(v)) {}
expr::identifier::identifier(ast::term::identifier&& id)
    : term(std::move(id)) {}
cmm::location expr::identifier::location() const { return term.location(); }
expr::literal::literal(const token& token)
    : term(token),
      type(token.type.get_properties().type.value()) {}

cmm::location expr::literal::location() const { return term.location(); }
expr::call::call(decltype(ident)&& ident_, decltype(args)&& args)
    : ident(std::move(ident_)),
      args(std::move(args)) {
  // ident.set_parent(this);
  args.set_parent(this);
}
cmm::location expr::call::location() const { return ident.location() + args.location(); }

expr::unary_operator::unary_operator(expression& expression, term::operator_&& op)
    : expr(expression),
      operator_(std::move(op)) {
  expr.set_parent(this);
  operator_.set_parent(this);
}
cmm::location expr::unary_operator::location() const {
  return expr.location() + operator_.location();
}
cmm::location expr::binary_operator::location() const {
  return left.location() + operator_.location() + right.location();
}
using namespace decl;

label::label(const token& label_)
    : term(label_) {}

variable::variable(decl::specifiers&& mods, decltype(ident) id, decltype(init) i)
    : specifiers(std::move(mods)),
      ident(id),
      init(i) {
  specifiers.set_parent(this);
  if (ident != nullptr) {
    ident->set_parent(this);
  }
  if (init != nullptr) {
    init->set_parent(this);
  }
}

cmm::location decl::label::location() const { return term.location(); }

cmm::location decl::variable::location() const {
  return specifiers.location() + GET_LOC(ident) + GET_LOC(init);
}
function::function(decl::specifiers&& mods,
                   decltype(ident)&& ident_,
                   decltype(parameters)&& args,
                   decltype(body) body_)
    : specifiers(std::move(mods)),
      ident(std::move(ident_)),
      parameters(std::move(args)),
      body(body_) {}

cmm::location decl::function::location() const {
  return specifiers.location() + ident.location() + parameters.location() + GET_LOC(body);
}

expr::binary_operator::binary_operator(expression& left, expression& right, term::operator_&& op)
    : left(left),
      right(right),
      operator_(std::move(op)) {
  left.set_parent(this);
  right.set_parent(this);
  operator_.set_parent(this);
}
selection::if_::if_(term::keyword&& k,
                    decltype(condition) condition,
                    decltype(block) block_,
                    decltype(else_) else_)
    : keyword(std::move(k)),
      condition(condition),
      block(block_),
      else_(else_) {
  keyword.set_parent(this);
  condition.set_parent(this);
  if (block != nullptr) {
    block->set_parent(this);
  }
  if (else_ != nullptr) {
    else_->set_parent(this);
  }
}

cmm::location selection::if_::location() const {
  return keyword.location() + condition.location() + GET_LOC(else_) + GET_LOC(block);
}

template <typename It>
std::string iteration::iteration<It>::condition_label() const {
  return std::format("cond_{}", static_cast<const It*>(this)->format());
}
template <typename It>
std::string iteration::iteration<It>::exit_label() const {
  return std::format("exit_{}", static_cast<const It*>(this)->format());
}

iteration::while_::while_(term::keyword&& k, expr::expression& condition_, statement* block)
    : keyword(std::move(k)),
      condition(condition_),
      body(block) {
  keyword.set_parent(this);
  condition.set_parent(this);
  if (body != nullptr) {
    body->set_parent(this);
  }
}

cmm::location iteration::while_::location() const {
  return keyword.location() + condition.location() + GET_LOC(body);
}

iteration::for_::for_(term::keyword&& k,
                      decl::variable* start_,
                      expr::expression* condition_,
                      expr::expression* step_,
                      statement* block)
    : keyword(std::move(k)),
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
  if (step != nullptr) {
    step->set_parent(this);
  }
}

cmm::location iteration::for_::location() const {
  return keyword.location() + GET_LOC(condition) + GET_LOC(body) + GET_LOC(start) + GET_LOC(step);
}

jump::goto_::goto_(const token& token)
    : term(token) {
  term.set_parent(this);
}
cmm::location jump::goto_::location() const { return term.location(); }

jump::break_::break_(const token& token)
    : keyword(token) {
  keyword.set_parent(this);
}
cmm::location jump::break_::location() const { return keyword.location(); }
jump::continue_::continue_(const token& token)
    : keyword(token) {
  keyword.set_parent(this);
}
cmm::location jump::continue_::location() const { return keyword.location(); }
jump::return_::return_(term::keyword k, expr::expression* expr_)
    : keyword(std::move(k)),
      expr(expr_) {
  keyword.set_parent(this);
}
cmm::location jump::return_::location() const { return keyword.location() + GET_LOC(expr); }

} // namespace cmm::ast
