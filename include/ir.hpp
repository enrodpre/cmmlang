#pragma once

#include "allocator.hpp"
#include "asm.hpp"
#include "ast.hpp"
#include "common.hpp"
#include "lang.hpp"
#include "semantic.hpp"
#include "traverser.hpp"
#include <algorithm>
#include <cstdint>
#include <libassert/assert.hpp>
#include <optional>
#include <type_traits>
#include <unordered_map>
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

struct compilation_unit;

class mangled_name {
public:
  using value_type = std::string;
  explicit mangled_name(const value_type&);
  mangled_name(value_type&&);

  static mangled_name variable(cstring, cr_type);
  static mangled_name label(cstring);
  static mangled_name function(cstring, const std::vector<ast::decl::variable>&);
  static mangled_name function(cstring, const std::vector<ptr_type>&);
  static mangled_name direct_conversion_function(cr_type, cr_type);
  static std::string types(const std::vector<const type*>&);

  [[nodiscard]] const value_type& str() const;
  operator std::string() const;

private:
  value_type m_string;
};

struct scope;
struct local_scope;

template <typename T>
concept is_declaration = std::is_base_of_v<ast::decl::declaration, T>;

template <typename Decl>
  requires(is_declaration<Decl>)
struct symbol : public formattable {
  using decl_t       = Decl;
  ~symbol() override = default;
  ir::scope* scope;
  ir::operand* address;

  [[nodiscard]] virtual bool is_loaded() const noexcept {
    return scope == nullptr || address == nullptr;
  }
  [[nodiscard]] virtual cr_string identifier() const noexcept = 0;
  void load(ir::scope* s, operand* addr);
  [[nodiscard]] const decl_t* declaration() const { return m_decl; }

  symbol(const Decl*);

private:
  const Decl* m_decl;
};

struct variable;
struct user_function;

struct label : public symbol<cmm::ast::decl::label> {
  label(const ast::decl::label*, local_scope*, assembly::label_memory*);
  [[nodiscard]] std::string format() const override;
  [[nodiscard]] cr_string identifier() const noexcept override {
    return declaration()->term.value();
  }
};

// struct array : public symbol<array, ast::decl::array> {
//   array(const ast::decl::label* label_);
//   [[nodiscard]] std::string format() const override;
// };

static_assert(std::is_move_constructible<label>());
static_assert(std::is_move_assignable<label>());

template <typename T>
concept is_var_declaration =
    std::is_same_v<std::remove_cvref_t<std::remove_pointer_t<std::decay_t<T>>>,
                   ast::decl::variable>;

static_assert(is_var_declaration<const ast::decl::variable*&>);

struct variable : public symbol<ast::decl::variable> {
  linkage_t linkage;
  storage_t storage;
  ptr_type type;
  variable(const ast::decl::variable*, linkage_t, storage_t, decltype(type));
  [[nodiscard]] std::string format() const override;
  [[nodiscard]] cr_string identifier() const noexcept override {
    return declaration()->ident->value();
  };
};

template <typename T>
concept is_func_declaration =
    std::is_same_v<std::remove_pointer_t<std::remove_cvref_t<T>>, ast::decl::function>;

struct function : public symbol<ast::decl::function> {
  struct signature_t {
    std::string name;
    std::vector<ptr_type> argument_types;

    signature_t(std::string name, std::vector<ptr_type> args)
        : name(std::move(name)),
          argument_types(std::move(args)) {}
    operator std::tuple<cstring, std::vector<ptr_type>>() const {
      return std::make_tuple(name, argument_types);
    }
    bool operator==(const signature_t& other) const {
      return name == other.name &&
             std::ranges::all_of(std::views::zip(argument_types, other.argument_types),
                                 [](const auto& type_pair) {
                                   const auto& [t, other_t] = type_pair;
                                   return *t == *other_t;
                                 });
    }
    [[nodiscard]] mangled_name mangle() const {
      return mangled_name::function(name, argument_types);
    }
  };

  struct argument;

  using return_t = ptr_type;
  linkage_t linkage;
  return_t return_type;
  bool inlined;

  function(const decl_t*, linkage_t, return_t, bool);
  [[nodiscard]] std::string format() const override;
  virtual std::vector<operand*> load(compilation_unit&,
                                     const std::vector<ast::expr::expression*>& = {}) const  = 0;
  virtual std::optional<operand*> run(compilation_unit&, const std::vector<operand*>&) const = 0;
  std::optional<operand*> load_and_run(compilation_unit&,
                                       const std::vector<ast::expr::expression*>& = {}) const;
  [[nodiscard]] virtual bool is_defined() const                        = 0;
  [[nodiscard]] virtual std::vector<ptr_type> argument_types() const   = 0;
  [[nodiscard]] [[nodiscard]] virtual bool is_builtin() const noexcept = 0;
};

namespace builtin::function {
  struct builtin_signature_t;
}

struct builtin_function : public function {
  using return_t     = std::optional<operand*>;
  using parameters_t = std::vector<ptr_type>;
  using preprocess_t =
      std::function<std::vector<operand*>(compilation_unit&,
                                          parameters_t,
                                          const std::vector<ast::expr::expression*>&)>;
  using body_t        = std::function<operand*(compilation_unit&, std::vector<operand*>)>;
  using postprocess_t = std::function<return_t(compilation_unit&, operand*)>;

  struct descriptor_t {
    preprocess_t pre;
    body_t body;
    postprocess_t post;
    descriptor_t(body_t b);
    descriptor_t(preprocess_t, body_t b);
    descriptor_t(preprocess_t, body_t b, postprocess_t);
  };

  signature_t signature;
  descriptor_t descriptor;
  builtin_function(cr_string, ptr_type, const parameters_t&, descriptor_t&&, bool = false);
  builtin_function(signature_t&&, ptr_type, descriptor_t&&, bool = false);

  std::vector<operand*> load(compilation_unit&,
                             const std::vector<ast::expr::expression*>&) const override;
  std::optional<operand*> run(compilation_unit&, const std::vector<operand*>& = {}) const override;

  [[nodiscard]] bool is_defined() const override;
  [[nodiscard]] cr_string identifier() const noexcept override { return signature.name; };
  [[nodiscard]] std::vector<ptr_type> argument_types() const override;
  [[nodiscard]] bool is_builtin() const noexcept override { return true; };
};

struct user_function : public function {
  const ast::compound* body;
  user_function(const ast::decl::function*, linkage_t, cr_type, bool = false);

  std::vector<operand*> load(compilation_unit&,
                             const std::vector<ast::expr::expression*>&) const override;
  std::optional<operand*> run(compilation_unit&, const std::vector<operand*>& = {}) const override;
  [[nodiscard]] bool is_defined() const override;
  [[nodiscard]] cr_string identifier() const noexcept override {
    return declaration()->ident.value();
  };
  [[nodiscard]] std::vector<ptr_type> argument_types() const override;
  [[nodiscard]] bool is_builtin() const noexcept override { return false; };
};

enum class conversion_operand_t : uint8_t {
  ANY_TYPE,
  ANY_FUNDAMENTAL,
};
struct conversion_operand {};
struct conversion_function {
  using body_t = std::variant<std::monostate, builtin_function::body_t, const ast::compound*>;
  enum class conversion_type_t : uint8_t { IMPLICIT, EXPLICIT };
  conversion_type_t type;
  body_t body;

  conversion_function(body_t);
  virtual ~conversion_function()                                    = default;
  [[nodiscard]] virtual bool is_convertible(cr_type) const noexcept = 0;
  [[nodiscard]] virtual cr_type to(cr_type) const noexcept          = 0;
  [[nodiscard]] virtual operand* operator()(operand*) const noexcept;
  [[nodiscard]] bool is_implicit() const { return type == conversion_type_t::IMPLICIT; };
  [[nodiscard]] bool is_explicit() const { return type == conversion_type_t::EXPLICIT; };
};

struct direct_conversion_function : public conversion_function {
  cr_type from_type;
  cr_type to_type;

  [[nodiscard]] bool is_convertible(cr_type t) const noexcept override { return from_type == t; }
  [[nodiscard]] cr_type to(cr_type) const noexcept override { return to_type; };

  direct_conversion_function(decltype(body), cr_type, cr_type);
};
struct glob_conversion_function : public conversion_function {
  std::string description;
  using condition_t = std::function<bool(cr_type)>;
  using extractor_t = std::function<cr_type(cr_type)>;
  condition_t condition;
  extractor_t extractor;

  [[nodiscard]] bool is_convertible(cr_type t) const noexcept override { return condition(t); }
  [[nodiscard]] cr_type to(cr_type t) const noexcept override { return extractor(t); };

  glob_conversion_function(std::string, body_t, condition_t&&, extractor_t&&);
};

template <typename T>
struct symbol_result {};
class variable_store : public formattable_range<std::unordered_map<std::string, variable>> {
public:
  using key_type   = std::string;
  using value_type = variable;
  variable_store();
  [[nodiscard]] bool contains(const std::string&) const;
  size_t size() const noexcept;
  value_type* get(const std::string&);
  [[nodiscard]] const value_type* get(const std::string&) const;
  void put(value_type&&);
  template <typename T, typename... Args>
    requires std::is_constructible_v<T, Args...>
  value_type& emplace(key_type, Args&&...);
  void clear();

  friend scope;
  friend local_scope;

private:
  std::unordered_map<key_type, value_type> m_store;
};

class function_store
    : public formattable_range<std::unordered_map<std::string, std::unique_ptr<function>>> {
public:
  using key_type       = mangled_name::value_type;
  using value_type     = std::unique_ptr<function>;
  using container_type = std::unordered_map<key_type, value_type>;
  function_store();
  const container_type& data() const { return m_store; }
  container_type::const_iterator begin() const { return m_store.begin(); }
  container_type::const_iterator end() const { return m_store.end(); }
  container_type::const_iterator cbegin() const { return m_store.begin(); }
  container_type::const_iterator cend() const { return m_store.cend(); }
  bool contains(const mangled_name& t) const { return m_store.contains(t); };
  const function* get(const function::signature_t&) const;
  std::vector<const function*> get_by_name(cstring) const;

  const function* emplace_builtin(std::string&&,
                                  ptr_type,
                                  const std::vector<ptr_type>&,
                                  builtin_function::descriptor_t&&,
                                  bool = false);
  const function* emplace_user_provided(const ast::decl::function*, bool = false);
  void clear();

private:
  container_type m_store;
};

class conversion_store {
public:
  using key_type     = mangled_name::value_type;
  using value_type   = std::unordered_map<key_type, direct_conversion_function>;

  conversion_store() = default;
  bool is_convertible(const type&, const type&) const;
  std::vector<ptr_type> get_convertible_types(const type&) const;
  std::vector<const conversion_function*> get_conversions(const type&) const;

  void emplace_direct(conversion_function::body_t, cr_type, cr_type);
  void emplace_glob(std::string,
                    conversion_function::body_t,
                    glob_conversion_function::condition_t,
                    glob_conversion_function::extractor_t);

private:
  std::unordered_map<key_type, value_type> m_direct_store;
  std::vector<glob_conversion_function> m_glob_store;
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

  operand* create_builtin(const ast::decl::variable*, linkage_t, storage_t, ptr_type, operand*);
  operand* create_user_provided(const ast::decl::variable*, operand*);

private:
  operand* create(const ast::decl::variable*, linkage_t, storage_t, ptr_type, operand*);
};

struct global_scope : public scope {};

struct local_scope : public scope {
  cref<frame> frame_ref;
  cref<ast::compound> compound;
  local_scope(const frame&, const ast::compound&);
  void load_argument(variable);
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
  void push_frame(const user_function*);
  size_t pop_frame();

  template <typename T>
  bool is_declared(const ast::term::identifier&) const noexcept;
  template <typename T>
  bool is_declarable(const ast::term::identifier&) const noexcept;
  const label* get_label(const ast::term::identifier&) const;
  const variable* get_variable(const ast::term::identifier&) const;
  const function* get_function(const function::signature_t&) const;
  const function* get_function(const ast::term::identifier&) const;
  void declare_function(const ast::decl::function*, bool = false);
  template <typename T>
  T* load_symbol(const ast::term::term&);

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
  conversion_store m_conversions;
  global_scope m_global_scope;
  stack<frame> m_stackframe;
  memory::Allocator m_arena;

  std::optional<const function*> progressive_prefix_match(
      const std::vector<ptr_type>&,
      const std::vector<const function*>&) const;
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

  //////////// OBJECTS ///////////
  [[nodiscard]] std::string current_line() const;

  const variable* declare_local_variable(const ast::decl::variable&, operand*);
  const variable* declare_global_variable(const ast::decl::variable&, operand*);
  void declare_label(const ast::decl::label&);
  [[nodiscard]] operand* get_variable_address(const ast::term::identifier&) const;
  void save_variable(const variable*, operand*);
  void reserve_memory(cstring, cstring, cstring);
  std::optional<operand*> call_builtin(const builtin::function::builtin_signature_t&,
                                       const std::vector<operand*>&);

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

namespace builtin {
  namespace function {
    enum class _builtin_signature_t : uint8_t { EXIT };
    using header_arguments_t = std::vector<ptr_type>;
    struct builtin_signature_t : public cmm::enumeration<_builtin_signature_t> {
      BUILD_ENUMERATION(builtin_signature_t,
                        std::string_view,
                        function_name,
                        header_arguments_t,
                        args);

      [[nodiscard]] ir::function::signature_t signature() const {
        return {std::string(function_name), args};
      }
      [[nodiscard]] mangled_name mangle() const {
        return mangled_name::function(function_name, args);
      }
    };
    namespace preprocessors {
      inline extern const builtin_function::preprocess_t SIMPLE_LOADING;
      inline extern const builtin_function::preprocess_t RAX_LOADING;
    }; // namespace preprocessors
    namespace bodies {
      using body_t                = ir::builtin_function::body_t;
      constexpr auto EXECUTE_MOVE = [](compilation_unit& v,
                                       const std::vector<operand*>& regs) -> operand* {
        return v.move(regs[0], regs[1]);
      };

      template <_instruction_t Ins>
      constexpr auto EXECUTE_INSTRUC =
          [](compilation_unit& v, const std::vector<operand*>& regs) -> operand* {
        constexpr auto N = instruction_t(Ins).n_params;
        if constexpr (N == 0) {
          v.instruction(instruction_t(Ins));
          return nullptr;
        } else {
          auto* param = regs[0];
          if constexpr (N == 1) {
            v.instruction(instruction_t(Ins), param);
          } else if constexpr (N == 2) {
            auto* param2 = regs[1];
            v.instruction(instruction_t(Ins), param, param2);
          }
          return param;
        }
      };
      ;
      template <_instruction_t Ims>
      constexpr auto POST_UNARY =
          [](compilation_unit& v, const std::vector<operand*>& args) -> operand* {
        auto* arg          = args[0];
        auto* saved_before = v.move(v.regs.get(assembly::registers::ACCUMULATOR), arg);
        if (const auto* var = arg->variable()) {
          v.save_variable(var, arg);
          return saved_before;
        };
        UNREACHABLE("aaaa");
      };
      inline extern const body_t EXIT;
      inline extern const body_t CAST_TO_BOOL;
    }; // namespace bodies
    namespace postprocessors {
      using post_t = ir::builtin_function::postprocess_t;
      inline extern const post_t SIMPLE_RET;
      inline extern const post_t RET_RAX;
      inline extern const post_t SAVE_VARIABLE;
    }; // namespace postprocessors
  }; // namespace function
  class provider {
  public:
    symbol_table& table;
    constexpr void provide();

  private:
    constexpr void create_function(ptr_type,
                                   std::string&&,
                                   const std::vector<ptr_type>&,
                                   builtin_function::descriptor_t&&);
    constexpr void create_builtin_function(ptr_type,
                                           const function::builtin_signature_t&,
                                           builtin_function::descriptor_t&&);
    constexpr void create_builtin_operator(ptr_type,
                                           const operator_t&,
                                           const std::vector<ptr_type>&,
                                           builtin_function::descriptor_t&&);
    constexpr void create_direct_conversion(cr_type, cr_type, builtin_function::body_t);
    constexpr void create_glob_conversion(std::string,
                                          glob_conversion_function::condition_t,
                                          glob_conversion_function::extractor_t,
                                          const builtin_function::body_t&);

    template <_instruction_t Ins>
    constexpr void create_simple_operator(const operator_t& op, ptr_type type);
    constexpr void provide_operators();
    constexpr void provide_functions();
    constexpr void provide_conversions();
  };
} // namespace builtin
}; // namespace cmm::ir
//
#include "ir.inl"
