#pragma once

#include "allocator.hpp"
#include "asm.hpp"
#include "ast.hpp"
#include "common.hpp"
#include "lang.hpp"
#include "traverser.hpp"
#include <cstdint>
#include <libassert/assert.hpp>
#include <optional>
#include <type_traits>
#include <utility>

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

  // Order left to right
  template <address_intent_t R, address_mode_intent_t A, address_intent_t L>
  struct intent<R, A, L> {};

  // I want to have the memory address, address that address and then retrieve
  // the value
  struct _load_variable_value : intent<address_intent_t::HAVING_ADDRESS,
                                       address_mode_intent_t::ADDRESS_MEMORY,
                                       address_intent_t::HAVING_VALUE> {};

  struct _load_variable_address : intent<address_intent_t::HAVING_ADDRESS,
                                         address_mode_intent_t::COPY,
                                         address_intent_t::HAVING_ADDRESS> {};

  struct _move_value : intent<address_intent_t::HAVING_VALUE,
                              address_mode_intent_t::COPY,
                              address_intent_t::HAVING_VALUE> {};

  inline static constexpr _load_variable_value load_variable_value;
  inline static constexpr _load_variable_address load_variable_address;
  inline static constexpr _move_value move_value;

  enum class intent_t : uint8_t {
    MOVE_CONTENT,
    LOAD_VARIABLE_VALUE,
    LOAD_VARIABLE_ADDRESS,
    SAVE_VARIABLE_VALUE
  };
} // namespace intents
struct variable;
struct function;
struct label;
struct array;

struct compilation_unit;

template <typename Decl>
struct symbol : public formattable {
  using address_t = assembly::operand*;
  using decl_t    = Decl;
  const Decl* decl;
  address_t addr;

  symbol(const Decl* decl, address_t);
};

struct label : public symbol<ast::decl::label> {
  label(const decl_t* label_, address_t);
  [[nodiscard]] std::string format() const override;
};

struct array : public symbol<ast::decl::label> {
  array(const decl_t* label_, address_t);
  [[nodiscard]] std::string format() const override;
};

static_assert(std::is_move_constructible<label>());
static_assert(std::is_move_assignable<label>());

struct scope;
struct local_scope;

struct variable : public symbol<ast::decl::variable> {
  linkage_t linkage;
  storage_t storage;
  cv_type type;
  scope& scope_ref;
  variable(scope&, const ast::decl::variable*, address_t, linkage_t, storage_t, decltype(type));
  [[nodiscard]] std::string format() const override;
};

struct local_variable : public variable {
  local_variable(local_scope&, const ast::decl::variable*, operand*, storage_t, decltype(type));
};

struct auto_local_variable : public local_variable {
  auto_local_variable(local_scope&,
                      const ast::decl::variable*,
                      assembly::reg_memory*,
                      storage_t,
                      decltype(type));
};
struct arg_local_variable : public local_variable {
  arg_local_variable(local_scope&,
                     const ast::decl::variable*,
                     assembly::reg*,
                     storage_t,
                     decltype(type));
};

struct global_variable : public variable {
  global_variable(scope&, const ast::decl::variable*, assembly::label_memory*, decltype(type));
};

struct function : public symbol<ast::decl::function> {
  using return_t = std::optional<cv_type>;
  std::string identifier;
  linkage_t linkage;
  return_t return_type;
  bool inlined;
  function(const ast::decl::function*, address_t, std::string, linkage_t, return_t, bool);
  [[nodiscard]] std::string format() const override;
  virtual address_t run(compilation_unit&, ast::expr::call::arguments = {}) const = 0;
  virtual address_t run(compilation_unit&, std::vector<operand*>) const           = 0;
  [[nodiscard]] virtual bool is_defined() const                                   = 0;
};

class mangled_name {
public:
  using value_type = std::string;
  explicit mangled_name(const value_type&);
  mangled_name(value_type&&);

  static mangled_name free_unary_operator(const operator_t&, cstring);
  static mangled_name free_binary_operator(const operator_t&, cstring, cstring);
  static mangled_name free_function(const ast::decl::function*);
  static mangled_name builtin_function(value_type, const std::vector<cv_type>&);
  static std::string types(const std::vector<cv_type>&);

  [[nodiscard]] const value_type& str() const;
  operator std::string() const;

private:
  value_type m_string;
};

struct builtin_function : public function {
  using return_t     = std::optional<address_t>;
  using parameters_t = std::vector<cv_type>;
  using preprocess_t = std::function<
      std::vector<address_t>(compilation_unit&, parameters_t, ast::expr::call::arguments)>;
  using body_t        = std::function<operand*(compilation_unit&, std::vector<address_t>)>;
  using postprocess_t = std::function<return_t(compilation_unit&, address_t)>;

  struct descriptor_t {
    preprocess_t preprocess;
    body_t body;
    postprocess_t postprocess;

    descriptor_t(preprocess_t preprocess, body_t body, postprocess_t postprocess)
        : preprocess(std::move(preprocess)),
          body(std::move(body)),
          postprocess(std::move(postprocess)) {}
  };

  parameters_t parameters;
  descriptor_t descriptor;
  builtin_function(std::string, std::optional<cv_type>, parameters_t, descriptor_t, bool = false);

  address_t run(compilation_unit&, ast::expr::call::arguments) const override;
  address_t run(compilation_unit&, std::vector<operand*> = {}) const override;

  [[nodiscard]] bool is_defined() const override;
};

struct user_function : public function {
  using body_t = ast::compound*;
  body_t body;
  ast::decl::function::parameters_t parameters;
  user_function(const ast::decl::function*, address_t, linkage_t, cv_type, bool = false);

  address_t run(compilation_unit&, ast::expr::call::arguments) const override;
  address_t run(compilation_unit&, std::vector<operand*> = {}) const override;
  [[nodiscard]] bool is_defined() const override;
};

class variable_store : public formattable_range<std::unordered_map<std::string, variable>> {
public:
  using key_type   = std::string;
  using value_type = variable;
  variable_store();
  [[nodiscard]] bool contains(const std::string&) const;
  size_t size() const noexcept;
  value_type* get(const std::string&);
  [[nodiscard]] const value_type* get(const std::string&) const;
  template <typename T, typename... Args>
    requires std::is_constructible_v<T, Args...>
  const value_type& emplace(key_type, Args&&...);
  void clear();

  friend scope;
  friend local_scope;

private:
  std::unordered_map<key_type, value_type> m_store;
};

class function_store
    : public formattable_range<std::unordered_map<std::string, std::unique_ptr<function>>> {
public:
  using key_type   = mangled_name::value_type;
  using value_type = std::unique_ptr<function>;
  function_store();
  bool contains(const mangled_name&) const;
  value_type::pointer get(const mangled_name&);
  const value_type::pointer get(const mangled_name&) const;
  std::vector<value_type::pointer> get(cstring) const;

  const function* emplace_builtin(const mangled_name&,
                                  std::optional<cv_type>,
                                  const std::vector<cv_type>&,
                                  const builtin_function::descriptor_t&,
                                  bool = false);
  const function* emplace_user_provided(const ast::decl::function*, bool = false);
  void clear();

private:
  std::unordered_map<key_type, value_type> m_store;
};

namespace builtin {
  struct provider;
}

struct frame {
  // Struct for tracking ASM stack
  struct assembly_stack {
    // assembly_stack() = default;
    [[nodiscard]] size_t size() const { return stack_size; };
    void push() { stack_size++; };
    void pop(size_t times = 1) { stack_size -= times; }

  private:
    size_t stack_size = 0;
  };

  cref<user_function> func;
  stack<local_scope> scopes;
  assembly_stack local_stack;
  std::unordered_map<cstring, label> labels;

  frame(const user_function*);
  frame(frame&&) noexcept = default;
  frame(const frame&)     = default;

  local_scope& active_scope();
  [[nodiscard]] const local_scope& active_scope() const;
  void clear() noexcept;
  [[nodiscard]] bool is_declared(const ast::term::identifier&) const noexcept;
  [[nodiscard]] variable* get(const ast::term::identifier&);
  [[nodiscard]] const variable* get(const ast::term::identifier&) const;

  void create_scope(const ast::compound&) noexcept;
  size_t destroy_scope() noexcept;

  // std::string format() const override;
};

struct scope {
  virtual ~scope() = default;
  variable_store variables;
};

struct global_scope : public scope {
  operand* emplace_static(const ast::decl::variable*);
};

struct local_scope : public scope {
  operand* emplace_argument(const ast::decl::variable*, assembly::reg*);
  operand* emplace_automatic(const ast::decl::variable*);
  cref<frame> frame_ref;
  cref<ast::compound> compound;
  local_scope(const frame&, const ast::compound&);
  // [[nodiscard]] const frame* parent() const override;
};

static_assert(!std::is_abstract_v<global_scope>);
static_assert(!std::is_abstract_v<local_scope>);

struct symbol_table : public cmm::formattable {
  using identifier_type = const ast::term::identifier&;

  symbol_table();

  void clear() noexcept;
  [[nodiscard]] frame& active_frame() noexcept;
  [[nodiscard]] const frame& active_frame() const noexcept;
  [[nodiscard]] scope& active_scope() noexcept;
  [[nodiscard]] const scope& active_scope() const noexcept;

  // Frames
  void push_frame(const ast::term::identifier&);
  size_t pop_frame();

  template <typename T>
  bool is_declared(const ast::term::identifier&) const noexcept;
  template <typename T>
  bool is_declarable(const ast::term::identifier&) const noexcept;
  const label* get_label(const ast::term::identifier&) const;
  const variable* get_variable(const ast::term::identifier&) const;
  const function* get_function(const mangled_name&) const;
  const function* get_function(const ast::term::identifier&) const;
  void declare_function(const ast::decl::function*, bool = false);

  bool is_entry_point_defined() const noexcept;
  user_function* get_entry_point();
  void link_entry_point(const ast::decl::function*);
  [[nodiscard]] bool is_global_scope() const noexcept;
  bool in_main() const noexcept;

  std::string format() const override;

  friend compilation_unit;

private:
  std::unique_ptr<user_function> m_entry_point;
  function_store m_functions;
  global_scope m_global_scope;
  stack<frame> m_stackframe;
  memory::Allocator m_arena;

  friend builtin::provider;
  friend compilation_unit;
};

// Pruning is preventing the generation of instruction_ts
// that are held after a return in the same scope,
// since they are unreachable
// Exiting is when a terminating function is called, such as exit
enum class Phase : uint8_t {
  STOPPED = 0,
  GLOBAL,
  EXECUTING,
  PRUNING_FULL,  // Full prune of current branch
  PRUNING_LABEL, // Prune until last.pruning_branch label found
  EXITING
};

template <_instruction_t Ins, size_t N, typename... Args>
concept ValidParameterCount = (sizeof...(Args) == N);

template <_instruction_t Ins, size_t N>
concept FuncWithNParams = instruction_t(Ins).n_params == N;

namespace builtin::function {
  [[nodiscard]] std::string mangle();
}; // namespace builtin::function

class compilation_unit : public default_singleton<compilation_unit> {
public:
  compilation_unit();

  ast_traverser runner;
  std::string compile(ast::program&, const source_code*);

  /////////// OBJECTS //////////

  struct {
    std::optional<operator_t> operator_        = std::nullopt;
    const ast::term::identifier* pruning_label = nullptr;
  } last;
  struct {
    size_t whiles = 0;
    size_t fors   = 0;
  } counters;
  std::optional<instruction_t> next_jump;
  size_t next_offset  = 0;
  Phase current_phase = Phase::STOPPED;
  symbol_table table;
  location src_location;
  const cmm::ast::statement* current_statement{};
  const source_code* source = nullptr;
  assembly::registers regs;

  cmm::assembly::asmgen asmgen;

private:
  //////////// OBJECTS ///////////

public:
  [[nodiscard]] std::string current_line() const;

  const variable* declare_variable(const ast::decl::variable&, operand*);
  const variable* declare_global_variable(const ast::decl::variable&, operand*);
  void declare_label(const ast::decl::label&);
  [[nodiscard]] operand* get_variable_address(const ast::term::identifier&) const;
  void save_variable(const variable*, operand*);
  void reserve_memory(cstring, cstring, cstring);
  cv_type get_expression_type(const ast::expr::expression&);
  operand* call_builtin(const std::string&, std::vector<operand*>);

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
  void exit(size_t);
  void syscall();
  void syscall(cstring);
  void ret();
  void label(cstring);
  void comment(cstring);

private:
  void start();
  std::string end();
};

namespace builtin {
  struct provider {
    symbol_table& table;
    constexpr void create_builtin_function(std::optional<cv_type>,
                                           std::string,
                                           const std::vector<cv_type>&,
                                           builtin_function::preprocess_t,
                                           builtin_function::body_t,
                                           builtin_function::postprocess_t);
    constexpr void create_builtin_operator(cv_type,
                                           const operator_t&,
                                           const std::vector<cv_type>&,
                                           builtin_function::preprocess_t,
                                           builtin_function::body_t,
                                           builtin_function::postprocess_t);

    template <_instruction_t Ins, size_t = instruction_t(Ins).n_params>
    constexpr void create_simple_operator(const operator_t& op, cv_type type);
    constexpr void provide_operators();
    constexpr void provide_functions();
    constexpr void provide();
  };
} // namespace builtin
}; // namespace cmm::ir
//
#include "ir.inl"
