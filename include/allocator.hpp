#pragma once

#include "common.hpp"
#include "traits.hpp"
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <memory>
#include <new>
#include <utility>

namespace cmm::memory {

template <typename F, pointer_t T>
T cast_pointer(F f) {
  auto* v = static_cast<void*>(f);
  return static_cast<T>(v);
}

struct register_;

struct Allocator {
  Allocator();
  Allocator(size_t m_capacity, std::byte* m_buffer, std::byte* m_offset);
  ~Allocator() { delete[] m_buffer; }

  [[nodiscard]] size_t used() const { return (m_offset - m_buffer) * sizeof(int); }

  template <typename T>
  [[nodiscard]] T free_space_as() const {}

  bool contains(void* ptr) const noexcept { return m_buffer < ptr && ptr < m_offset; }

  template <typename T>
  [[nodiscard]] T* allocate(size_t times = 1) {
    size_t remaining_space = m_capacity - used();
    auto* start_ptr        = static_cast<void*>(m_offset);
    size_t element_size    = sizeof(T);
    if (remaining_space < element_size) {
      throw std::bad_alloc();
    }

    size_t total_size           = element_size * times;
    auto* const aligned_address = std::align(alignof(T), total_size, start_ptr, remaining_space);
    if (nullptr == aligned_address) {
      throw std::bad_alloc();
    }

    auto allocated_pointer = static_cast<T*>(aligned_address);

    // registers.emplace_back(
    //     type_name<T>(), (uintptr_t)allocated_pointer, sizeof(T), times);

    return allocated_pointer;
  }

  template <typename T>
  [[nodiscard]] T* allocate_array(T& t) {
    const auto allocated_memory = allocate<T>();
    std::uninitialized_move(std::begin(t), std::end(t), allocated_memory);
    return allocated_memory;
  }

  template <typename T, typename... Args>
    requires std::constructible_from<T, Args...>
  [[nodiscard]] T* emplace(Args&&... args) {
    return new (allocate<T>()) T(std::forward<Args>(args)...);
  }

  void reset() { m_offset = m_buffer; }

  template <typename T>
  void destroy(T* ptr) {
    ptr->~T();
  }

  void report_statistics() const noexcept;

  struct register_ {
    std::string node;
    uintptr_t start;
    size_t size, times;
    bool deleted = false;
  };

  std::vector<register_> registers;

private:
  static constexpr size_t ALLOCATOR_SIZE = static_cast<size_t>(1024 * 1024 * 8);
  size_t m_capacity                      = ALLOCATOR_SIZE;
  std::byte* m_buffer = new std::byte[ALLOCATOR_SIZE]; // Where objects are stored
  std::byte* m_offset = m_buffer;
};

} // namespace cmm::memory
