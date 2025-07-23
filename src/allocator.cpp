#include "allocator.hpp"

namespace cmm::memory {

Allocator::Allocator()
    : m_capacity{ALLOCATOR_SIZE},
      m_buffer{new std::byte[ALLOCATOR_SIZE]},
      m_offset{m_buffer} {}

Allocator::Allocator(size_t m_capacity,
                     std::byte* m_buffer,
                     std::byte* m_offset)
    : m_capacity(m_capacity),
      m_buffer(m_buffer),
      m_offset(m_offset) {}

void Allocator::report_statistics() const noexcept {
  auto accumulate_stat = [](const register_& a,
                            const register_& b) -> register_ {
    return {.size = a.size + b.size, .times = a.times + b.times};
  };

  register_ total{};
  size_t n_opfunctions = 0;
  uintptr_t last_end   = 0;
  for (const auto& reg : registers) {
    uintptr_t end = (reg.size + reg.start);
    if (last_end != 0 && last_end != end) {
      // strings::colorizer::colorize<strings::colorizer::style_t::RED>(color);
      // log::apply(color, log::style_t::RED);
    }
    REGISTER_DEBUG("{}: {}, {}-{}",
                   reg.node,
                   reg.size,
                   (const void*)reg.start,
                   (const void*)end);

    last_end = end;
  }
}

} // namespace cmm::memory
