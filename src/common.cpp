#include "common.hpp"
#include <libassert/assert-macros.hpp>
#include <magic_enum/magic_enum.hpp>
#include <string>

#define TYPE_INDEX(cls) std::type_index(typeid(cls))
namespace cmm {

self_allocated::self_allocated(cmm::location&& loc)
    : m_location(std::move(loc)) {}
self_allocated::self_allocated(const cmm::location& loc)
    : m_location(loc) {}

[[nodiscard]] std::string location::format() const { return std::format("({}, {})", start, end); }

source_code::source_code(const fs::ifile& input)
    : m_filename(input.filename()),
      m_code(input.content()) {}

[[nodiscard]] const std::string& source_code::get_code() const { return m_code; }

[[nodiscard]] const std::string& source_code::get_filename() const { return m_filename; }

bool source_code::is_valid(const location& loc) const {
  auto line                = get_line(loc);
  const auto& [start, end] = loc;
  return (start <= line.first) && (end <= line.second);
}

std::pair<size_t, size_t> source_code::get_line(const location& loc) const {
  const auto& [start, end] = loc;

  // Get last \n before start
  auto right_before = m_code.substr(0, start).rfind('\n');
  if (right_before == std::string::npos) {
    throw cmm::error("aa");
  }

  auto right_after = m_code.substr(end, std::string::npos).find('\n');
  if (right_after == std::string::npos) {
    throw cmm::error("aa");
  }

  return std::make_pair(right_before, end + right_after);
}

std::string source_code::get_chunk(const location& loc) const {
  if (!is_valid(loc)) {
    auto line = get_line(loc);
    REGISTER_ERROR("location {}, size {}", loc, line.second - line.first);
    throw std::exception();
  }

  auto [start, len] = loc;
  return m_code.substr(start, start + len);
}

std::tuple<std ::string, std::string, std::string> source_code::get_line_chunked(
    const location& loc) const {
  auto [start, end]                 = loc;
  const auto& [top_left, top_right] = get_line(loc);
  auto left                         = m_code.substr(top_left, start - top_left);
  auto middle                       = m_code.substr(start, (end - start));
  auto right                        = m_code.substr(end, top_right - end);

  return {left, middle, right};
}

string_buffer::string_buffer() { m_actives.emplace_back(); }

string_buffer& string_buffer::operator<<(string_buffer& other) {
  active() << other.dump();
  return *this;
}

string_buffer& string_buffer::operator<<(const std::string& str) {
  active() << str;
  return *this;
}

void string_buffer::create() { m_actives.emplace_back(); }

void string_buffer::save() {
  ASSERT(!m_actives.empty(), "WARN! Been tryed to save a pointer where there are no stacked");
  auto to_save = std::move(m_actives.top());
  m_actives.pop();
  m_saved.push(std::move(to_save));
}

void string_buffer::load() {
  ASSERT(m_saved.empty(), "WARN! Been tryed to load a saved pointer where there was no saved");
  m_actives.top() << m_saved.top().str();
  m_saved.pop();
}

std::string string_buffer::dump() {
  ASSERT(!m_actives.empty(), "Trying to dump when no buffers active");
  return m_actives.pop_return().str();
}

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
cstring string_buffer::snapshot() const noexcept { return active().view(); }

namespace {
auto cast_to_ins(operator_sign sign, comparison_t c, std::string str, std::string prefix) {
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
