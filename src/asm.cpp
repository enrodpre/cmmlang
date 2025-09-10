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

std::string operand::string() const { return value(); }

[[nodiscard]] std::optional<operand::symbol_container> operand::content() const { return m_symbol; }

[[nodiscard]] types::type_id operand::content_type() const {
  return content().value().content->specs.type.value();
};

[[nodiscard]] operand::content_t operand::variable() const { return m_symbol.value().content; }

operand::symbol_container::symbol_container(content_t cont, symbol_attr attr)
    : content(cont),
      attribute(attr),
      m_disposable(false) {}

bool operand::symbol_container::is_address() const { return attribute == symbol_attr::ADDRESS; }

operand* operand::hold_value(content_t obj) {
  m_symbol.emplace(obj, symbol_container::symbol_attr::VALUE);
  return this;
}

operand* operand::hold_address(content_t obj) {
  m_symbol.emplace(obj, symbol_container::symbol_attr::ADDRESS);
  return this;
}

void operand::release() { m_symbol.reset(); }

bool operand::is_writtable() const {
  return !m_symbol.has_value() || m_symbol.value().is_disposable();
}

[[nodiscard]] bool operand::empty() const { return !m_symbol.has_value(); }

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

template <register_t Reg>
register_placeholder::register_placeholder()
    : reg(std::format("placeholder_{}", Reg)),
      bound_reg(ir::compilation_unit::instance().regs.get(Reg)) {}

std::string stack_memory::value() const {
  auto& v                 = ir::compilation_unit::instance();
  uint64_t current_casted = 0;
  if (v.ast->is_global_scope()) {
    current_casted = static_cast<int64_t>(v.ast->active_frame()->active_scope()->variables.size());
  } else {
    current_casted = static_cast<int64_t>(v.ast->active_frame()->local_stack.size());
  }
  auto offset = calculate_offset(m_offset, static_cast<int64_t>(current_casted));
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

[[nodiscard]] std::string label_literal::label_length() const {
  return std::format("{}_len", value());
}

stack_memory* operand_factory::create_stack_memory(uint64_t i) { return create<stack_memory>(i); }

label* operand_factory::create_label(const std::string& s) { return create<label>(s); }

label_memory* operand_factory::create_label_memory(std::string&& s) {
  return create<label_memory>(std::move(s));
}

label_literal* operand_factory::create_label_literal(std::string&& s) {
  return create<label_literal>(std::move(s));
}

registers::registers()
    : m_registers(initialize_registers()) {}

[[nodiscard]] reg* registers::get(register_t name) const {
  return m_registers.at(magic_enum::enum_index(name).value());
}

const ast::decl::variable* registers::find_var(const ast::identifier& id) {
  auto range = m_registers | TRANSFORM([](const auto* r) { return r->variable(); }) |
               FILTER([id](const auto* var) { return var->ident.value() == id.value(); }) | TO_VEC;
  if (range.empty()) {
    return nullptr;
  }
  return range.front();
}

registers::parameters_transaction registers::parameters() { return {this}; }

reg* registers::parameters_transaction::next() {
  const auto* it = std::ranges::find_if(
      m_parameters, [this](register_t r) { return params->get(r)->is_writtable(); });
  if (it != m_parameters.end()) {
    auto* available = params->get(*it);
    m_regs.push_back(available);
    return available;
  }
  throw cmm::error("No more registers available");
}

void registers::parameters_transaction::reset() {
  for (auto* r : m_regs) {
    r->release();
  }
}

static_assert(std::formattable<instruction_t, char>);

asmgen::asmgen() {
  m_sections.procedures.emplace(std::make_pair("exit", "mov rax, 60\n  syscall"));
}

void asmgen::start() {}

std::string asmgen::end() {

  string_buffer res;
  if (!m_sections.bss.empty()) {
    res << "section .bss\n";
    for (const auto& line : m_sections.bss) {
      res << "  " << line << "\n";
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
    res << std::format("{}:\n  {}\n", fn.first, fn.second);
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
      // m_sections.procedures.emplace_back(pair);
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
