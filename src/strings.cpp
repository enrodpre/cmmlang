#include "strings.hpp"
#include "common.hpp"
#include "fs.hpp"
#include <cstddef>
#include <exception>
#include <utils.hpp>

using namespace cmm;
using namespace strings;

size_t strings::find_next_newline(cstring data) {
  size_t pos = data.find('\n');
  return pos;
}

bool strings::match_regex_sv(
    cstring where,
    cstring pattern,
    std::match_results<cstring::const_iterator>& result) {
  std::regex re(pattern.data());
  return std::regex_match(where.cbegin(), where.cend(), result, re);
}

using buffer_type    = strings::string_buffer::buffer_type;
using reference_type = strings::string_buffer::reference_type;
using constant_type  = strings::string_buffer::constant_type;

std::string strings::generate_compilation_message(const source_code& src,
                                                  cstring message,
                                                  const location& loc) {
  auto text_header = std::format(
      "{}:{}:{}:", src.get_filename(), loc.rows.start, loc.cols.start);
  auto styled_text_header = colorizer::header_style(text_header);

  auto first_line         = std::format("{} {} {}",
                                styled_text_header,
                                colorizer::error_style("error: "),
                                message);

  auto second_line        = std::format(
      "   {} |    {}", loc.rows.start, strings::highlight(src, loc));
  return std::format("{}\n{}", first_line, second_line);
}

source_code::source_code(const fs::ifile& input)
    : m_filename(input.filename()),
      m_code(input.content()) {}

[[nodiscard]] const std::string& source_code::get_code() const {
  return m_code;
}

[[nodiscard]] const std::string& source_code::get_filename() const {
  return m_filename;
}

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
    spdlog::error(
        "location {}, size {}", loc, get_line(loc.rows.start).size());
    throw std::exception();
  }

  auto [start, len] = loc.cols;
  auto line         = get_line(loc.rows.start);
  return line.substr(start, start + len);
}

std::tuple<std ::string, std::string, std::string>
source_code::get_line_chunked(const location& loc) const {
  auto [start, len] = loc.cols;
  auto line         = get_line(loc.rows.start);
  auto left         = line.substr(0, start);
  auto middle       = line.substr(start, len);
  auto right        = line.substr(start + len);

  return {left, middle, right};
}

strings::string_buffer& strings::string_buffer::operator<<(
    string_buffer& other) {
  active() << other.dump();
  return *this;
}

strings::string_buffer& strings::string_buffer::operator<<(
    const std::string& str) {
  active() << str;
  return *this;
}

void strings::string_buffer::create() {
  m_actives.emplace_back();
}

void strings::string_buffer::save() {
  ASSERT(!m_actives.empty(),
         "WARN! Been tryed to save a pointer where there are no stacked");
  auto to_save = std::move(m_actives.top());
  m_actives.pop();
  m_saved.push(std::move(to_save));
}

void strings::string_buffer::load() {
  ASSERT(m_saved.empty(),
         "WARN! Been tryed to load a saved pointer where there was no saved");
  m_actives.top() << m_saved.top().str();
  m_saved.pop();
}

std::string strings::string_buffer::dump() {
  ASSERT(!m_actives.empty(), "Trying to dump when no buffers active");
  return m_actives.pop_return().str();
}

std::string strings::string_buffer::flush() {
  while (!m_saved.empty()) {
    load();
  }
  return dump();
}

reference_type strings::string_buffer::active() noexcept {
  return m_actives.top();
}

[[nodiscard]] const buffer_type& strings::string_buffer::active()
    const noexcept {
  return m_actives.top();
}
cstring strings::string_buffer::snapshot() const noexcept {
  return active().view();
}
