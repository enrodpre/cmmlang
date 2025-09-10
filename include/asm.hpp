#pragma once

#include <algorithm> // for __count_if_fn, count_if
#include <array>     // for array
#include <cstddef>   // for size_t
#include <cstdint>   // for uint8_t, int64_t, uint64_t
#include <format>    // for format_string, format
#include <memory>    // for unique_ptr
#include <optional>  // for optional
#include <string>    // for basic_string, string, char_traits
#include <string_view>
#include <type_traits> // for remove_cvref_t, is_abstract_v, remove_cvref
#include <utility>     // for pair, make_pair, forward
#include <variant>     // for variant
#include <vector>      // for vector

#include "ast.hpp"    // for identifier, variable
#include "common.hpp" // for std::string_view, formattable, DATASIZE, string_buffer
#include "macros.hpp" // for BUILD_ENUMERATION_DATA_CLASS, CTOR_ASSIGN_DATA_4
#include "types.hpp"  // for type

namespace cmm {
enum class instruction_t : uint8_t;
} // namespace cmm

namespace cmm::assembly {

namespace bits {
constexpr auto BIT_DATASIZE = DATASIZE * 8;

namespace masks {
constexpr auto TO_BOOL = 0x1;
}
} // namespace bits

enum class syscall_t : uint8_t {
  READ,
  WRITE,
  EXIT,
};

BUILD_ENUMERATION_DATA_CLASS(syscall, size_t, n_param, size_t, number)

struct operand : displayable {
  using content_t = const ast::decl::variable*;

  struct symbol_container {
    enum symbol_attr : uint8_t { VALUE, ADDRESS };

    content_t content;
    symbol_attr attribute;

    symbol_container(content_t, symbol_attr);
    symbol_container(const symbol_container&)            = default;
    symbol_container& operator=(const symbol_container&) = default;
    [[nodiscard]] bool is_address() const;

    [[nodiscard]] bool is_disposable() const noexcept { return m_disposable; }

    void set_disposable() noexcept { m_disposable = true; }

  private:
    bool m_disposable;
  };

  using container_t = std::optional<symbol_container>;

  enum class type_t : uint8_t { REGISTER, IMMEDIATE, MEMORY, LABEL };

  // enum class size : uint8_t {
  //   BYTE  = 8,  // 8-bit
  //   WORD  = 16, // 16-bit
  //   DWORD = 32, // 32-bit
  //   QWORD = 64, // 64-bit
  // };

  [[nodiscard]] virtual type_t type() const       = 0;
  [[nodiscard]] virtual std::string value() const = 0;
  [[nodiscard]] std::string string() const override;

  [[nodiscard]] std::optional<symbol_container> content() const;
  [[nodiscard]] types::type_id content_type() const;
  [[nodiscard]] content_t variable() const;
  operand* hold_value(content_t);
  operand* hold_address(content_t);
  [[nodiscard]] bool empty() const;
  [[nodiscard]] bool is_writtable() const;
  void release();

protected:
  std::optional<symbol_container> m_symbol;
};

template <typename T>
concept Operand =
    std::is_base_of_v<operand, std::remove_cvref_t<std::remove_pointer_t<std::remove_cvref_t<T>>>>;

static_assert(Operand<operand*&>);

struct memory : public operand {
  [[nodiscard]] type_t type() const override { return type_t::MEMORY; }
};

struct reg : public operand {
  reg(std::string);
  NOT_COPYABLE_CLS(reg);
  NOT_MOVABLE_CLS(reg);

  [[nodiscard]] type_t type() const override { return type_t::REGISTER; }

  [[nodiscard]] std::string name() const { return m_name; }

  [[nodiscard]] std::string value() const override;

protected:
  std::string m_name;
};

struct register_placeholder : public reg {
  register_placeholder()
      : reg(std::format("placeholder_{}", i++)) {}

  template <register_t Reg>
  register_placeholder();

  void bind(reg*);
  reg* bound_reg{};
  static inline int i = 1;
};

struct reg_memory : public reg {
  reg_memory(std::string, int64_t);
  using reg::hold_address;
  using reg::hold_value;

  [[nodiscard]] type_t type() const override { return type_t::MEMORY; }

  [[nodiscard]] std::string value() const override;

protected:
  int64_t m_offset;
};

struct stack_memory : public reg_memory {
  stack_memory(int64_t);
  [[nodiscard]] std::string value() const override;
};

struct immediate : public operand {
  using stored_t = std::variant<int64_t, uint64_t, float, double, long double>;
  enum class immediate_t : uint8_t { SIGNED_INTEGER, UNSIGNED_INTEGER, FLOAT, DOUBLE, LONG_DOUBLE };

  immediate(stored_t, immediate_t);

  [[nodiscard]] type_t type() const override { return type_t::IMMEDIATE; }

  [[nodiscard]] immediate_t immediate_type() const { return m_immediate_type; }

  [[nodiscard]] std::string value() const override;

protected:
  stored_t m_value;
  immediate_t m_immediate_type;
};

struct immediate_memory : public immediate {
  using immediate::immediate;

  [[nodiscard]] type_t type() const override { return type_t::MEMORY; }

  [[nodiscard]] std::string value() const override;
};

struct label : public virtual operand {
  label(std::string);

  [[nodiscard]] type_t type() const override { return type_t::LABEL; }

  [[nodiscard]] std::string value() const override;

protected:
  std::string m_name;
};

struct label_memory : public label {
  using label::hold_address;
  using label::hold_value;
  using label::label;

  [[nodiscard]] type_t type() const override { return type_t::MEMORY; }

  [[nodiscard]] std::string value() const override;
};

struct label_literal : public label {
  using label::hold_address;
  using label::hold_value;
  using label::label;
  [[nodiscard]] std::string label_length() const;
};

struct operand_factory {
  STATIC_CLS(operand_factory);
  static stack_memory* create_stack_memory(uint64_t);
  static label* create_label(const std::string&);
  static label_memory* create_label_memory(std::string&&);
  static label_literal* create_label_literal(std::string&&);
  template <typename V, typename... Args>
    requires std::is_constructible_v<V, Args...>
  static V* create(Args&&...);
};

struct registers {
  using value_type = reg* const;
  using store_type = std::array<value_type, 11>;

  registers();

  constexpr static std::string to_realname(register_t);
  [[nodiscard]] reg* get(register_t) const;

  [[nodiscard]] size_t available_parameters() const {
    return std::ranges::count_if(m_parameters,
                                 [this](register_t r) { return get(r)->is_writtable(); });
  }

  // reg* last_opfunction_result;
  const ast::decl::variable* find_var(const ast::identifier&);

  [[nodiscard]] constexpr reg* parameter_at(int i) const;

  struct parameters_transaction {
    parameters_transaction(registers* p)
        : params(p) {}

    ~parameters_transaction() { reset(); }

    reg* next();
    void reset();

  private:
    registers* params;
    std::vector<reg*> m_regs;
  };

  constexpr static const std::array<register_t, 6> m_parameters = {register_t::SYSCALL_1,
                                                                   register_t::SYSCALL_2,
                                                                   register_t::SCRATCH_1,
                                                                   register_t::SCRATCH_4,
                                                                   register_t::SCRATCH_2,
                                                                   register_t::SCRATCH_3};

  registers::parameters_transaction parameters();

private:
  store_type m_registers;

  constexpr static store_type initialize_registers();
};

// Usage example
static_assert(!std::is_abstract_v<label>);
static_assert(!std::is_abstract_v<reg_memory>);
static_assert(!std::is_abstract_v<immediate_memory>);
static_assert(!std::is_abstract_v<label_memory>);
static_assert(!std::is_abstract_v<reg>);
static_assert(!std::is_abstract_v<immediate>);

struct assembly_code : public displayable {};

struct assembly_line : public displayable {};

struct assembly_empty_line : public assembly_line {
  [[nodiscard]] std::string string() const override;
};

struct assembly_comment_line : public assembly_line {
  std::string comment;
  assembly_comment_line(std::string);

  [[nodiscard]] std::string string() const override;
};

struct assembly_code_line : public assembly_line {
  std::unique_ptr<assembly_code> instruction;
  std::string comment;

  assembly_code_line(decltype(instruction)&&, decltype(comment));

  [[nodiscard]] std::string string() const override;
};

class comment_block;

using constant_data_descriptor = std::pair<assembly::label*, assembly::label*>;

class asmgen {
public:
  enum class Section : uint8_t { TEXT = 0, DATA, BS };

  asmgen();
  void start();
  std::string end();

  // Main generators
  void write_label(std::string_view);
  void write_comment(std::string_view) noexcept;
  template <typename... Args>
  void write_instruction(const instruction_t&, Args&&...);
  void add_data(std::string_view, std::string_view);
  void add_bss(std::string_view, std::string_view, std::string_view);
  void save_current_procedure();
  void load_new_procedure(std::string_view);
  [[nodiscard]] static bool exists_snippet(std::string_view);
  void include_snippet(std::string_view);

  // Helpers
  template <typename... Args>
  [[nodiscard]] comment_block begin_comment_block(std::format_string<Args...> std, Args&&... args);

  // Dynamic buffer
  // void create_delay();
  // [[nodiscard]] std::string dump_delayed();
  // void stop_delay();
  // void load_delayed();

private:
  std::unique_ptr<std::pair<std::string, std::string>> m_current_procedure;
  std::vector<std::string> m_comment_blocks;

  struct {
    std::unordered_map<std::string, std::string> procedures;
    std::vector<std::string> bss;
    std::unordered_map<std::string, std::string> data;
  } m_sections;

  constexpr static std::array procedures_snippets = {
      std::make_pair("print_argc",
                     "  pop  ecx\n  add  ecx, '0'\n  push  ecx\n  mov  ecx, esp\n  mov eax, 4\n  "
                     "mov  ebx, 1\n  mov edx, 1\n syscall"),
      std::make_pair(
          "print",
          "  mov rax, 1\n  mov rdi, 1\n  lea rsi, [newline]\n  mov rdx, 1\n  syscall\n  ret")};
};

class comment_block {
public:
  comment_block(asmgen&, std::string);
  ~comment_block();
  void end();

private:
  asmgen& m_asmgen;
  std::string m_name;
  bool m_ended = false;
};

template <typename... Args>
comment_block asmgen::begin_comment_block(std::format_string<Args...> std, Args&&... args) {
  std::string comment = std::format(std, std::forward<Args>(args)...);
  m_comment_blocks.emplace_back(comment);
  write_comment(comment);
  return {*this, comment};
}

enum class Snippet : uint8_t { int_to_str, exit, print_str, print_nl, print_int };
constexpr static const std::string_view PRINT_NL = ""
                                                   "syscall\n  ret";
constexpr static const std::string_view INT_TO_FORMAT =
    R"(  mov rcx, 10                ; Base 10
  xor rbx, rbx               ; Clear rbx (used for digit count)

; Clear the buffer (optional, but recommended)
  mov rsi, rdi               ; Copy buffer pointer to rsi
  mov byte [rsi], 0          ; Null-terminate the string (optional)

.convert_loop:
  xor rdx, rdx               ; Clear rdx (remainder)
  div rcx                    ; Divide rax by 10
  add dl, '0'                ; Convert remainder to ASCII
  dec rdi                    ; Move buffer pointer back
  mov [rdi], dl              ; Store the character
  inc rbx                    ; Increment digit count
  test rax, rax              ; Check if rax is zero
  jnz .convert_loop          ; Repeat if not zero
  mov rax, rdi               ; Return the pointer to the start  
ret)";
constexpr static const std::string_view PRINT_INT =
    R"(  ;; Value to print should be in rax register
  lea rdi, [num + 10]         ;Pointer to the buffer
  call int_to_str

  mov rax, 1
  mov rdi, 1
  lea rsi, [num + 10]               ;Pointer to the number string
  sub rsi, rbx               ;Find the first character
  mov rdx, rbx               ;Length of the number string
  syscall
  ret)";

}; // namespace cmm::assembly

#include "asm.inl"
