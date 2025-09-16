#include "asm.hpp"

#include <algorithm>
#include <magic_enum/magic_enum.hpp>
#include <optional>
#include <string_view>
#include <utility>

#include "asm.inl"
#include "ast.hpp"
#include "ir.hpp"

namespace cmm::assembly {

std::string element::string() const { return value(); }

operand* operand::hold_value() {
  m_content  = VALUE;
  m_variable = std::nullopt;
  return this;
}

operand* operand::hold_symbol(const ast::decl::variable* t_var) {
  m_content  = ADDRESS;
  m_variable = t_var;
  return this;
}

reg::reg(std::string name)
    : m_name(std::move(name)) {}

namespace {
std::string format_offset(uint64_t offset) {
  return std::format(" {}{}", offset > 0 ? "+ " : "", offset * DATASIZE);
}

std::string format_addr(const std::string& v, uint64_t offset) {
  return std::format("[{}{}]", v, offset != 0 ? format_offset(offset) : "");
};

uint64_t calculate_offset(uint64_t original_size, uint64_t current_size) {
  return (-current_size) - (-original_size) + 1;
}

} // namespace

std::string stack_memory::value() const {
  auto current_casted =
      symbol().value()->get_root()->active_frame()->active_scope()->variables.size();
  auto offset = calculate_offset(m_offset, current_casted);
  return format_addr(m_base, offset);
}

label::label(std::string name)
    : m_name(std::move(name)) {}

[[nodiscard]] std::string label_memory::value() const { return format_addr(m_base, m_offset); }

[[nodiscard]] reg* registers::get(register_t name) const { return m_registers.at(name).get(); }

std::optional<reg*> registers::find_var(const ast::identifier& id) {
  auto range = m_registers | FILTER([id](const auto& r) {
                 return !r->empty() && r->symbol().value()->ident.value() == id.value();
               }) |
               std::views::transform([](const auto& t_reg) { return t_reg.get(); }) | TO_VEC;

  if (range.empty()) {
    return {};
  }
  return range.front();
}

registers::parameters_transaction registers::parameters() { return {this}; }

reg* registers::parameters_transaction::next() {
  const auto* it =
      std::ranges::find_if(m_parameters, [this](register_t r) { return !params->get(r)->empty(); });
  if (it != m_parameters.end()) {
    auto* available = params->get(*it);
    m_regs.push_back(available);
    return available;
  }
  throw cmm::error("No more registers available");
}

void registers::parameters_transaction::reset() {
  for (auto* r : m_regs) {
    r->reset();
  }
}

asmgen::asmgen() { m_sections.procedures.emplace("exit", "mov rax, 60\n  syscall"); }

void asmgen::start() {}

std::string asmgen::end() {

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
      const auto& [name, value] = line;
      res << std::format("  {} db \'{}\', 10", name, value) << "\n";
      res << std::format("  {}_len equ $ - {}", name, name) << "\n";
    }
    res.newline();
  }

  res << "section .text\n  global _start\n\n";
  res << "_start:\n";
  for (const auto& fn : m_sections.procedures) {
    res << std::format("{}:\n{}\n", fn.first, fn.second);
  }

  return res.dump();
}

void asmgen::add_data(std::string_view name, std::string_view value) {
  m_sections.data.emplace(name, value);
}

void asmgen::add_bss(std::string_view ident, std::string_view type, std::string_view size) {
  m_sections.bss.push_back(std::format("{} {} {}", ident, type, size));
}

void asmgen::save_current_procedure() {
  assert(m_current_procedure);
  m_sections.procedures.emplace(std::move(*m_current_procedure.release()));
}

void asmgen::load_new_procedure(std::string_view t_new_name) {
  assert(!m_current_procedure);
  m_current_procedure =
      std::make_unique<std::pair<std::string, std::string>>(std::string(t_new_name), std::string());
}

// void asmgen::create_delay() { m_current_procedure->create(); }
//
// [[nodiscard]] std::string asmgen::dump_delayed() { return m_current_procedure->dump(); }
//
// void asmgen::stop_delay() {
//   m_current_procedure->save();
//   m_current_procedure->create();
// }
//
// void asmgen::load_delayed() { m_current_procedure->load(); }
//
void asmgen::write_label(std::string_view label) {
  m_current_procedure->second.append(std::format("{}:\n", label));
}

void asmgen::write_comment(std::string_view comment) noexcept {
  m_current_procedure->second.append(std::format(";; {}:\n", comment));
}

[[nodiscard]] bool asmgen::exists_snippet(std::string_view name) {
  return std::ranges::any_of(procedures_snippets,
                             [name](const auto& pair) { return pair.first == name; });
}

void asmgen::include_snippet(std::string_view name) {
  for (const auto& pair : procedures_snippets) {
    if (pair.first == name) {
      m_sections.procedures.emplace(pair);
    }
  }
}

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
