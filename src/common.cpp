#include "common.hpp"

#define TYPE_INDEX(cls) std::type_index(typeid(cls))
namespace cmm {

formattable::operator std::string() const { return format(); }

self_allocated::self_allocated(cmm::location&& loc)
    : m_location(std::move(loc)) {}
self_allocated::self_allocated(const cmm::location& loc)
    : m_location(loc) {}

[[nodiscard]] std::string location::format() const { return std::format("({}, {})", rows, cols); }

source_code::source_code(const fs::ifile& input)
    : m_filename(input.filename()),
      m_code(input.content()) {}

[[nodiscard]] const std::string& source_code::get_code() const { return m_code; }

[[nodiscard]] const std::string& source_code::get_filename() const { return m_filename; }

bool source_code::is_valid(const location& loc) const {
  auto line                = get_line(loc.rows.start);
  const auto& [start, len] = loc.cols;
  return (start <= line.size()) && (start + len <= line.size());
}

std::string source_code::get_line(size_t nth) const {
  std::stringstream ss{std::string(m_code)};
  std::string line;

  for (size_t i = 0; std::getline(ss, line) && i < nth; ++i) {}

  return line;
}

std::string source_code::get_chunk(const location& loc) const {
  if (!is_valid(loc)) {
    REGISTER_ERROR("location {}, size {}", loc, get_line(loc.rows.start).size());
    throw std::exception();
  }

  auto [start, len] = loc.cols;
  auto line         = get_line(loc.rows.start);
  return line.substr(start, start + len);
}

std::tuple<std ::string, std::string, std::string> source_code::get_line_chunked(
    const location& loc) const {
  auto [start, len] = loc.cols;
  auto line         = get_line(loc.rows.start);
  auto left         = line.substr(0, start);
  auto middle       = line.substr(start, len);
  auto right        = line.substr(start + len);

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
} // namespace cmm
