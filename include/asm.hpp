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
#include <sys/types.h>
#include <utility> // for pair, make_pair, forward
#include <vector>  // for vector

#include "ast.hpp"    // for identifier, variable
#include "common.hpp" // for std::string_view, formattable, DATASIZE, string_buffer
#include "macros.hpp" // for BUILD_ENUMERATION_DATA_CLASS, CTOR_ASSIGN_DATA_4

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

struct element : displayable {
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
};

struct immediate : public element {
  enum immediate_t : uint8_t { SIGNED_INTEGER, UNSIGNED_INTEGER, FLOAT, DOUBLE, LONG_DOUBLE };

  immediate(std::string t_value, immediate_t t_type = SIGNED_INTEGER)
      : m_value(std::move(t_value)),
        m_immediate_type(t_type) {}

  [[nodiscard]] type_t type() const final { return type_t::IMMEDIATE; }
  [[nodiscard]] immediate_t immediate_type() const { return m_immediate_type; }
  [[nodiscard]] std::string value() const override { return m_value; };

protected:
  std::string m_value;
  immediate_t m_immediate_type;
};

struct label : public element {
  label(std::string);

  [[nodiscard]] type_t type() const final { return type_t::LABEL; }
  [[nodiscard]] std::string value() const override { return m_name; };

protected:
  std::string m_name;
};

using offset_t = int64_t;
struct operand : public element {
  enum content_t : u_int8_t { EMPTY = 0, VALUE, ADDRESS };

  operand* hold_value();
  operand* hold_symbol(const ast::decl::variable*);
  [[nodiscard]] std::optional<const ast::decl::variable*> symbol() const { return m_variable; };
  [[nodiscard]] std::string deref() const {
    assert(m_content == ADDRESS);
    return std::format("[{}]", value());
  }
  [[nodiscard]] bool empty() const { return m_content == EMPTY || !m_variable.has_value(); }
  void reset() {
    m_content = EMPTY;
    m_variable.reset();
  }

protected:
  content_t m_content                                  = EMPTY;
  std::optional<const ast::decl::variable*> m_variable = std::nullopt;

  operand()                                            = default;
};

struct reg : public operand {
  reg(std::string);
  NOT_COPYABLE_CLS(reg);
  NOT_MOVABLE_CLS(reg);

  [[nodiscard]] type_t type() const final { return type_t::REGISTER; }
  [[nodiscard]] std::string name() const { return m_name; }
  [[nodiscard]] std::string value() const override { return name(); };

protected:
  std::string m_name;
};

struct memory : public operand {
  memory(std::string t_operand, offset_t t_offset)
      : m_base(std::move(t_operand)),
        m_offset(t_offset) {}
  [[nodiscard]] type_t type() const final { return type_t::MEMORY; }

protected:
  std::string m_base;
  offset_t m_offset;
};

struct stack_memory : public memory {
  stack_memory(offset_t t_offset)
      : memory("rsp", t_offset) {}

  [[nodiscard]] std::string value() const override;
};

struct label_memory : public memory {
  label_memory(std::string t_label, offset_t t_offset = 0)
      : memory(std::move(t_label), t_offset) {}
  [[nodiscard]] std::string value() const override;
};

struct registers : default_singleton<registers> {
  friend default_singleton<registers>;

  enum register_t : uint8_t {
    RSP,
    RBP,
    ACCUMULATOR,
    COUNTER,
    AUX,
    SYSCALL_1,
    SYSCALL_2,
    SCRATCH_1,
    SCRATCH_2,
    SCRATCH_3,
    SCRATCH_4
  };

  using value_type = std::unique_ptr<reg>;
  using store_type = std::array<reg, 11>;

  constexpr static std::string to_realname(register_t);
  [[nodiscard]] reg* get(register_t);
  [[nodiscard]] const reg* get(register_t) const;
  void reset() {
    for (auto& reg : m_registers) {
      reg.reset();
    }
  }

  [[nodiscard]] size_t available_parameters() const {
    return std::ranges::count_if(m_parameters, [this](register_t r) { return get(r)->empty(); });
  }

  std::optional<reg*> find_var(const ast::identifier&);

  struct parameters_transaction {
    parameters_transaction(registers* p)
        : params(p) {}

    ~parameters_transaction() { reset(); }
    NOT_COPYABLE_CLS(parameters_transaction)

    reg* next();
    void reset();

  private:
    registers* params;
    std::vector<register_t> m_transaction_regs;
  };

  constexpr static const std::array m_parameters = {register_t::SYSCALL_1,
                                                    register_t::SYSCALL_2,
                                                    register_t::SCRATCH_1,
                                                    register_t::SCRATCH_4,
                                                    register_t::SCRATCH_2,
                                                    register_t::SCRATCH_3};

  registers::parameters_transaction parameters();

protected:
  inline registers();

private:
  store_type m_registers;
};

class comment_block;

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
          "  mov rax, 1\n  mov rdi, 1\n  lea rsi, [newline]\n  mov rdx, 1\n  syscall\n  ret"),
      std::make_pair("iota",
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
ret)"),
      std::make_pair("int_to_string",
                     R"(  ;; Value to print should be in rax register
  lea rdi, [num + 10]         ;Pointer to the buffer
  call int_to_str

  mov rax, 1
  mov rdi, 1
  lea rsi, [num + 10]               ;Pointer to the number string
  sub rsi, rbx               ;Find the first character
  mov rdx, rbx               ;Length of the number string
  syscall
                      };
                     ret)")};
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

}; // namespace cmm::assembly

#include "asm.inl"
