#include "asm.hpp"
#include "ir.hpp"
#include <cstdint>
#include <libassert/assert.hpp>
#include <magic_enum/magic_enum.hpp>
#include <optional>
#include <unistd.h>
#include <utility>

namespace cmm::assembly {

std::string operand::format() const { return value(); }
[[nodiscard]] std::optional<operand::symbol_container> operand::content() const { return m_symbol; }

[[nodiscard]] operand::content_t operand::variable() const { return m_symbol.value().content; }

operand::symbol_container::symbol_container(content_t cont, symbol_attr attr)
    : content(cont),
      attribute(attr) {}

bool operand::symbol_container::is_address() const { return attribute == symbol_attr::ADDRESS; }
operand* operand::hold_value(content_t obj) {
  m_symbol.emplace(obj, symbol_container::symbol_attr::VALUE);
  return this;
}
operand* operand::hold_address(content_t obj) {
  m_symbol.emplace(obj, symbol_container::symbol_attr::ADDRESS);
  return this;
}

[[nodiscard]] bool operand::empty() const { return !m_symbol.has_value(); }

void operand::release() { m_symbol.reset(); }

reg::reg(std::string name)
    : m_name(std::move(name)) {}

namespace {
  template <typename T>
  constexpr std::string get_var(immediate::stored_t stored) {
    return std::to_string(*std::get_if<T>(&stored));
  }

  std::string format_offset(int64_t offset) {
    return std::format(" {}{}", offset > 0 ? "+ " : "- ", offset * DATASIZE);
  }

  std::string format_addr(const std::string& v, int64_t offset) {
    return std::format("[{}{}]", v, offset != 0 ? format_offset(offset) : "");
  };

  int64_t calculate_offset(int64_t original_size, int64_t current_size) {
    return current_size - original_size;
  }

} // namespace

[[nodiscard]] std::string reg::value() const {
  if (content().has_value() &&
      content().value().attribute == symbol_container::symbol_attr::ADDRESS) {
    return std::format("[{}]", m_name);
  }
  return m_name;
}

immediate::immediate(stored_t value, immediate_t type)
    : m_value(value),
      m_immediate_type(type) {}

[[nodiscard]] std::string immediate::value() const {
  switch (m_immediate_type) {
    case immediate_t::UNSIGNED_INTEGER:
      return get_var<uint64_t>(m_value);
    case immediate_t::FLOAT:
      return get_var<float>(m_value);
    case immediate_t::DOUBLE:
      return get_var<double>(m_value);
    case immediate_t::LONG_DOUBLE:
      return get_var<long double>(m_value);
    case immediate_t::SIGNED_INTEGER:
    default:
      return get_var<int64_t>(m_value);
  }
}
reg_memory::reg_memory(std::string base, int64_t offset)
    : reg(std::move(base)),
      m_offset(offset) {}

std::string reg_memory::value() const { return format_addr(m_name, m_offset); }

stack_memory::stack_memory(int64_t offset)
    : reg_memory("rsp", offset) {}

std::string stack_memory::value() const {
  const auto& v          = ir::compilation_unit::instance();
  int64_t current_casted = 0;
  if (v.table.is_global_scope()) {
    current_casted = static_cast<int64_t>(v.table.active_scope().variables.size());
  } else {
    current_casted = static_cast<int64_t>(v.table.active_frame().local_stack.size());
  }
  auto offset = calculate_offset(m_offset, current_casted);
  return format_addr(m_name, offset);
}

[[nodiscard]] std::string immediate_memory::value() const {
  return std::format("[{}]", immediate::value());
}

label::label(std::string name)
    : m_name(std::move(name)) {}

[[nodiscard]] std::string label::value() const {
  if (content().has_value() &&
      content().value().attribute == symbol_container::symbol_attr::ADDRESS) {
    return std::format("[{}]", m_name);
  }
  return m_name;
}

[[nodiscard]] std::string label_memory::value() const { return std::format("[{}]", m_name); }

registers::registers()
    : m_registers(initialize_registers()),
      parameters(*this) {}

registers::parameters_t::parameters_t(registers& regs)
    : regs(regs),
      i(0) {}

[[nodiscard]] reg* registers::get(registers_t name) const {
  return m_registers.at(magic_enum::enum_index(name).value());
}

const ir::variable* registers::find_var(const ast::term::identifier& id) {
  for (const auto* r : m_registers) {
    if (const auto* var = r->variable()) {
      if (*var->decl->ident == id) {
        return var;
      }
    }
  }
  return nullptr;
}

assembly_instruction0::assembly_instruction0(instruction_t ins)
    : instruction(std::move(ins)) {}

[[nodiscard]] std::string assembly_instruction0::format() const {
  return std::format("{}", instruction);
}
assembly_instruction1::assembly_instruction1(instruction_t ins, operand* l)
    : instruction(std::move(ins)),
      left(l) {}
[[nodiscard]] std::string assembly_instruction1::format() const {
  return std::format("{} {}", instruction, *left);
}

assembly_instruction2::assembly_instruction2(instruction_t ins, operand* l, operand* r)
    : instruction(std::move(ins)),
      left(l),
      right(r) {}
[[nodiscard]] std::string assembly_instruction2::format() const {
  return std::format("{} {}, {}", instruction, *left, *right);
}

[[nodiscard]] std::string assembly_empty_line::format() const { return ""; }

assembly_comment_line::assembly_comment_line(std::string str)
    : comment(std::move(str)) {}
std::string assembly_comment_line::format() const { return std::format("; {}", comment); }
assembly_code_line::assembly_code_line(decltype(instruction)&& ins, decltype(comment) comm)
    : instruction(std::move(ins)),
      comment(std::move(comm)) {}

[[nodiscard]] std::string assembly_code_line::format() const {
  return std::format("{}{}", *instruction, comment);
}

static_assert(std::formattable<instruction_t, char>);

void asmgen::start() {
  m_text.write("{}\n", "section .text").write<2>("{}\n", "global _start");

  write_label("_start");
}

std::string asmgen::end() {
  auto text = m_text.flush();
  for (const auto& fn : m_sections.procedures) {
    text = std::format("{}\n\n{}:\n  {}", text, fn.first, fn.second);
  }

  string_buffer res;
  if (!m_sections.bss.empty()) {
    res << "section .bss\n";
    for (const auto& line : m_sections.bss) {
      res << line << "\n";
    }
    res << "\n";
  }
  if (!m_sections.data.empty()) {
    res << "section .data\n";
    for (const auto& line : m_sections.data) {
      res << line << "\n";
    }
    res << "\n";
  }

  res << text;

  return res.dump();
}

void asmgen::add_section_data(Section section_enum, cstring ident, cstring type, cstring size) {
  switch (section_enum) {
    case Section::DATA:
      m_sections.data.emplace_back(std::format("{} {} {}", ident, type, size));
      return;
    case Section::BS:
      m_sections.bss.emplace_back(std::format("{} {} {}", ident, type, size));
      return;
    case Section::TEXT:
    default:
      UNREACHABLE("NOT IMPLEMENTED");
  }
}

void asmgen::register_labeled_code_block(cstring name, std::string&& asm_code) {
  m_sections.procedures.emplace_back(name, std::move(asm_code));
}

void asmgen::create_delay() { m_text.create(); }

[[nodiscard]] std::string asmgen::dump_delayed() { return m_text.dump(); }
void asmgen::stop_delay() {
  m_text.save();
  m_text.create();
}

void asmgen::load_delayed() { m_text.load(); }

void asmgen::write_label(cstring label) { m_text.newline().write("{}:\n", label); }

void asmgen::write_comment(cstring comment) noexcept { m_text.write(";; {}\n", comment); }

comment_block::comment_block(asmgen& gen, std::string name)
    : m_asmgen(gen),
      m_name(std::move(name)) {}

comment_block::~comment_block() { end(); }

void comment_block::end() {
  if (!m_ended) {
    m_asmgen.write_comment(std::format("end {}", m_name));
    m_ended = true;
  }
}
}; // namespace cmm::assembly
