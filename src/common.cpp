#include "common.hpp"

#include <magic_enum/magic_enum.hpp>
#include <string>

#include "common.inl"

namespace cmm {

self_allocated::self_allocated(cmm::location&& loc)
    : m_location(std::move(loc)) {}
self_allocated::self_allocated(const cmm::location& loc)
    : m_location(loc) {}

[[nodiscard]] std::string location::string() const { return std::format("({}, {})", start, end); }

string_buffer::string_buffer() { m_actives.emplace(); }

string_buffer& string_buffer::operator<<(string_buffer& other) {
  active() << other.dump();
  return *this;
}

string_buffer& string_buffer::operator<<(const std::string& str) {
  active() << str;
  return *this;
}

void string_buffer::create() { m_actives.emplace(); }

void string_buffer::save() {
  auto to_save = std::move(m_actives.top());
  m_actives.pop();
  m_saved.push(std::move(to_save));
}

void string_buffer::load() {
  m_actives.top() << m_saved.top().str();
  m_saved.pop();
}

std::string string_buffer::dump() { return m_actives.pop_value().str(); }

std::string string_buffer::flush() {
  while (!m_saved.empty()) {
    load();
  }
  return dump();
}

string_buffer::reference_type string_buffer::active() noexcept { return m_actives.top(); }

[[nodiscard]] const string_buffer::buffer_type& string_buffer::active() const noexcept {
  return m_actives.top();
}
std::string_view string_buffer::snapshot() const noexcept { return active().view(); }

namespace {
auto cast_to_ins(operator_sign sign, comparison_t c, std::string_view str, std::string prefix) {
  if (sign == operator_sign::UNSIGNED && (c == comparison_t::U_LT || c == comparison_t::U_GT)) {
    str = str.substr(2);
  }
  return magic_enum::enum_cast<instruction_t>(std::format("{}{}", prefix, str),
                                              magic_enum::case_insensitive)
      .value();
}
} // namespace
instruction_t comparison_data::jump() const { return cast_to_ins(sign, m_value, name(), "j"); }
instruction_t comparison_data::set() const { return cast_to_ins(sign, m_value, name(), "set"); }

}; // namespace cmm
