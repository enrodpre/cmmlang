#include "ast.hpp"
#include "common.hpp"
#include <algorithm>
#include <ranges>
#include <type_traits>
#include <utility>
#include <utils.hpp>

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

std::optional<cmm::location> operator+(const std::optional<cmm::location>& lhs,
                                       const cmm::location& rhs) {
  if (lhs) {
    return *lhs + rhs;
  }
  return std::nullopt;
}

std::optional<cmm::location> operator+(const cmm::location& lhs,
                                       const std::optional<cmm::location>& rhs) {
  if (rhs) {
    return lhs + *rhs;
  }
  return std::nullopt;
}

std::optional<cmm::location> operator+(const std::optional<cmm::location>& lhs,
                                       const std::optional<cmm::location>& rhs) {
  if (lhs && rhs) {
    return *lhs + *rhs;
  }
  return std::nullopt;
}

#define CTOR_PARAMS_2(t1, n1)                         t1 _##n1
#define CTOR_PARAMS_4(t1, n1, t2, n2)                 t1 _##n1, t2 _##n2
#define CTOR_PARAMS_6(t1, n1, t2, n2, t3, n3)         t1 _##n1, t2 _##n2, t3 _##n3
#define CTOR_PARAMS_8(t1, n1, t2, n2, t3, n3, t4, n4) t1 _##n1, t2 _##n2, t3 _##n3, t4 _##n4
#define CTOR_PARAMS_10(t1, n1, t2, n2, t3, n3, t4, n4, t5, n5) \
  t1 _##n1, t2 _##n2, t3 _##n3, t4 _##n4, t5 _##n5
#define CTOR_PARAMS_12(t1, n1, t2, n2, t3, n3, t4, n4, t5, n5, t6, n6) \
  t1 _##n1, t2 _##n2, t3 _##n3, t4 _##n4, t5 _##n5, t6 _##n6

#define ADD_FMT_0(lvl, a1)
#define ADD_FMT_1(lvl, a1) \
  \n {}
#define ADD_FMT_2(lvl, a1, a2) \
  \n {} \
  \n {}
#define ADD_FMT_3(lvl, a1, a2, a3) \
  \n {} \
  \n {} \
  \n {}

#define FORMAT_ARGS_0(lvl, a1)
#define FORMAT_ARGS_1(lvl, a1)         a1.repr(lvl + 1)
#define FORMAT_ARGS_2(lvl, a1, a2)     a1.repr(lvl + 1), a2.repr(lvl + 1)
#define FORMAT_ARGS_3(lvl, a1, a2, a3) a1.repr(lvl + 1), a2.repr(lvl + 1), a3.repr(lvl + 1)

#define CTOR_ADD_FMT(...) CONCAT(ADD_FMT_, GET_ARG_COUNT(__VA_ARGS__))(__VA_ARGS__)
#define CTOR_FORMAT_ARGS(lvl, ...) \
  CONCAT(FORMAT_ARGS_, GET_ARG_COUNT(__VA_ARGS__))(lvl, __VA_ARGS__)

#define INDENT_IMPL(TYPE, stdstr, ...) \
  std::string TYPE::repr(size_t n) const { \
    return std::format("{}{}", std::string(n * 2, ' '), std::format(stdstr, __VA_ARGS__)); \
  } \
  std::string TYPE::format() const { return std::format(stdstr, __VA_ARGS__); }
#define FORMAT_INDENT_IMPL(TYPE, stdstr, ...) \
  std::string TYPE::repr(size_t n) const { \
    return std::format( \
        "{}{}", std::string(n * 2, ' '), std::format(stdstr, CTOR_FORMAT_ARGS(n, __VA_ARGS__))); \
  } \
  std::string TYPE::format() const { return std::format(stdstr, __VA_ARGS__); }

#define JOIN_INDENT_IMPL(TYPE, NAME, DELIM) \
  std::string TYPE::repr(size_t n) const { \
    return std::format( \
        "{}{}", std::string(2, ' '), std::format("{}({}):\n{}", NAME, size(), join(DELIM, n))); \
  } \
  std::string TYPE::format() const { return cpptrace::demangle(typeid(this).name()); }

std ::string compound ::format() const { return cpptrace::demangle(typeid(this).name()); }
std ::string compound ::repr(size_t n) const {
  return std ::format("{}{}",
                      std ::string(n * 2, ' '),
                      std ::format("{}({}):\n{}", "Compound", size(), join('\n', n)));
}
std ::string expr ::call ::arguments ::repr(size_t n) const {
  return std ::format("{}{}",
                      std ::string(2, ' '),
                      std ::format("{}({}):\n{}", "Arguments", size(), join('\n', n)));
}
std ::string expr ::call ::arguments ::format() const {
  return cpptrace ::demangle(typeid(this).name());
}
JOIN_INDENT_IMPL(decl::function::parameters_t, "Parameters", '\n')
JOIN_INDENT_IMPL(decl::specifiers, "Specifiers", ' ')
std ::string program ::repr(size_t n) const {
  std::string res;
  for (const global_statement* decl : *this) {
    res += decl->repr(n + 1) + '\n';
  }
  return std ::format(
      "{}{}", std ::string(2, ' '), std ::format("{}({}):\n{}", "Program", size(), res));
}
std ::string program ::format() const { return cpptrace ::demangle(typeid(this).name()); }
FORMAT_INDENT_IMPL(expr::identifier, "Identifier:\n{}", term);
FORMAT_INDENT_IMPL(expr::literal, "Literal:\n{}", term);
FORMAT_INDENT_IMPL(expr::unary_operator, "Unary:\n{}\n{}", operator_, expr);
FORMAT_INDENT_IMPL(expr::binary_operator, "Binary:\n{}\n{}\n{}\n", operator_, left, right);
FORMAT_INDENT_IMPL(expr::call, "Call:\n{}\n{}\n", ident, args);
FORMAT_INDENT_IMPL(decl::label, "Label:\n{}", term);
FORMAT_INDENT_IMPL(decl::variable, "Variable:\n{}", specifiers);
std ::string decl ::function ::repr(size_t n) const {
  auto params = parameters.transform(
                    [n](const decl::variable& param) -> std::string { return param.repr(n + 1); }) |
                std::views::join_with('\n') | std::ranges::to<std::string>();
  auto elems =
      body->transform([n](const statement* stmt) -> std::string { return stmt->repr(n + 1); }) |
      std::views::join_with('\n') | std::ranges::to<std::string>();
  auto res = std::format("{}{}",
                         std::string(n * 2, ' '),
                         std::format("Function:\n{}\n{}\n{}", ident.repr(n + 1), params, elems));
  return res;
};
std::string decl::function::format() const { return cpptrace::demangle(typeid(this).name()); }
std ::string iteration ::while_ ::repr(size_t n) const {
  return std ::format(
      "{}{}", std ::string(n * 2, ' '), std ::format("While:\n{}", condition.repr(n + 1)));
}
std ::string iteration ::while_ ::format() const { return "while"; };
std ::string iteration ::for_ ::repr(size_t n) const {
  return std ::format(
      "{}{}", std ::string(n * 2, ' '), std ::format("For:\n{}", (*condition).repr(n + 1)));
}
std ::string iteration ::for_ ::format() const { return "for"; };
std ::string selection ::if_ ::repr(size_t n) const {
  return std ::format("{}{}",
                      std ::string(n * 2, ' '),
                      std ::format("If:\n{}\n{}\n", condition.repr(n + 1), (*block).repr(n + 1)));
}
std ::string selection ::if_ ::format() const { return "if"; };
INDENT_IMPL(jump::break_, "{}", "Break");
INDENT_IMPL(jump::continue_, "{}", "Continue");
FORMAT_INDENT_IMPL(jump::return_, "Return:\n{}", (*expr));
FORMAT_INDENT_IMPL(jump::goto_, "Goto({})", term);

expr::expression::expression()
    : semantics() {}

void expr::expression::load_semantics(ptr_type t, value_category_t v) const {
  semantics.loaded         = true;
  semantics.original_type  = t;
  semantics.value_category = v;
}
leaf::leaf(cmm::location loc)
    : m_location(std::move(loc)) {}

compound::compound(std::vector<statement*>&& v)
    : siblings(std::move(v)) {}
expr::identifier::identifier(ast::term::identifier&& id)
    : term(std::move(id)) {}
std::optional<cmm::location> expr::identifier::location() const { return term.location(); }

expr::literal::literal(const token& token)
    : term(token),
      type(&type::create(token.type.get_properties().type.value())) {}

std::optional<cmm::location> expr::literal::location() const { return term.location(); }
expr::call::call(decltype(ident)&& ident_, decltype(args)&& args)
    : ident(std::move(ident_)),
      args(std::move(args)) {
  // ident.set_parent(this);
  args.set_parent(this);
}
std::optional<cmm::location> expr::call::location() const {
  return ident.location() + args.location();
}

expr::unary_operator::unary_operator(expression* expression, term::operator_&& op)
    : expr(*expression),
      operator_(std::move(op)) {
  expr.set_parent(this);
  operator_.set_parent(this);
}
std::optional<cmm::location> expr::unary_operator::location() const {
  return expr.location() + operator_.location();
}
std::optional<cmm::location> expr::binary_operator::location() const {
  return left.location() + operator_.location() + right.location();
}
using namespace decl;

bool expr::expression::is_category(value_category_t cat) const {
  return semantics.value_category == cat;
}
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

std::optional<cmm::location> decl::label::location() const { return term.location(); }

std::optional<cmm::location> decl::variable::location() const {
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

std::optional<cmm::location> decl::function::location() const {
  return specifiers.location() + ident.location() + parameters.location() + GET_LOC(body);
}

expr::binary_operator::binary_operator(expression* left, expression* right, term::operator_&& op)
    : left(*left),
      right(*right),
      operator_(std::move(op)) {
  left->set_parent(this);
  right->set_parent(this);
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

std::optional<cmm::location> selection::if_::location() const {
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

std::optional<cmm::location> iteration::while_::location() const {
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

std::optional<cmm::location> iteration::for_::location() const {
  return keyword.location() + GET_LOC(condition) + GET_LOC(body) + GET_LOC(start) + GET_LOC(step);
}

jump::goto_::goto_(const token& token)
    : term(token) {
  term.set_parent(this);
}
std::optional<cmm::location> jump::goto_::location() const { return term.location(); }

jump::break_::break_(const token& token)
    : keyword(token) {
  keyword.set_parent(this);
}
std::optional<cmm::location> jump::break_::location() const { return keyword.location(); }
jump::continue_::continue_(const token& token)
    : keyword(token) {
  keyword.set_parent(this);
}
std::optional<cmm::location> jump::continue_::location() const { return keyword.location(); }
jump::return_::return_(term::keyword k, expr::expression* expr_)
    : keyword(std::move(k)),
      expr(expr_) {
  keyword.set_parent(this);
}
std::optional<cmm::location> jump::return_::location() const {
  return keyword.location() + GET_LOC(expr);
}

linkage_t decl::specifiers::parse_linkage() const {
  auto res = std::ranges::find_if(
      cbegin(), cend(), [](const auto& spec) { return spec.type == token_t::static_; });
  if (res != cend()) {
    return linkage_t::internal;
  }
  return linkage_t::normal;
}

storage_t decl::specifiers::parse_storage() const {
  auto storages = data() |
                  std::views::filter([](const auto& spec) { return spec.type.is_storage(); }) |
                  std::ranges::to<std::vector>();
  if (storages.size() == 0) {
    return storage_t::normal;
  }
  if (storages.size() == 1) {
    return storages[0].type.cast<storage_t>();
  }

  throw_error<error_t::INCOMPATIBLE_TOKEN>(storages[1], storages[1], storages[0]);
}
namespace {
  constexpr type_category_t parse_enum_type(const token_t& token_type, bool unsigned_) {
    if (token_type == token_t::int_t) {
      return unsigned_ ? type_category_t::uint_t : type_category_t::sint_t;
    }
    return token_type.cast<type_category_t>();
  }
}; // namespace
cr_type decl::specifiers::parse_type() const {
  bool const_    = false;
  bool volatile_ = false;
  bool unsigned_ = false;
  std::optional<token_t> type_;
  for (const auto& t : *this) {
    if (t.type == token_t::const_) {
      const_ = true;
    } else if (t.type == token_t::volatile_) {
      volatile_ = true;
    } else if (t.type.is_type()) {
      type_.emplace(t.type);
    } else if (t.type == token_t::unsigned_) {
      unsigned_ = true;
    }
  }

  if (!type_.has_value()) {
    auto r = *this | std::views::transform([](const auto& spec) -> std::optional<cmm::location> {
      return spec.location();
    }) | std::ranges::to<std::vector>();

    throw_error<error_t::REQUIRED_TYPE>(*this);
  }
  return type::create_fundamental(parse_enum_type(type_.value(), unsigned_), const_, volatile_);
}
} // namespace cmm::ast
