#pragma once

#include "common.hpp"
#include <cstddef>
#include <cstdint>
#include <memory>
#include <new>
#include <string> // for basic_string, string
#include <type_traits>
#include <utility>
#include <vector> // for vector

namespace cmm::memory {

struct register_;

struct arena;

template <typename T>
class allocator {
public:
  using value_type = T;
  allocator() noexcept;

  template <typename U>
  allocator(const allocator<U>&) noexcept;

  T* allocate(std::size_t = 1);

  void deallocate(T* p, std::size_t n) noexcept {}

  template <typename U>
  bool operator==(const allocator<U>& rhs) const noexcept {
    return m_region == rhs.m_region;
  }

  template <typename U>
  bool operator!=(const allocator<U>& rhs) const noexcept {
    return !(*this == rhs);
  }

private:
  template <typename U>
  friend class allocator;
  arena& m_region;
};

struct arena : default_singleton<arena> {
  arena() = default;
  arena(size_t, std::byte*, std::byte*);
  ~arena() { delete[] m_buffer; }

  [[nodiscard]] size_t used() const { return static_cast<size_t>(m_offset - m_buffer); }

  template <typename T>
  [[nodiscard]] T free_space_as() const {}

  bool contains(void* ptr) const noexcept { return m_buffer < ptr && ptr < m_offset; }

  template <typename T>
  [[nodiscard]] T* allocate(size_t times = 1) {
    size_t remaining_space = m_capacity - used();
    size_t element_size    = sizeof(T) * times;
    if (remaining_space < element_size) {
      throw std::bad_alloc();
    }

    auto* start_ptr             = static_cast<void*>(m_offset);
    auto* const aligned_address = std::align(alignof(T), element_size, start_ptr, remaining_space);
    if (nullptr == aligned_address) {
      throw std::bad_alloc();
    }

    m_offset = static_cast<std::byte*>(aligned_address) + element_size;
    // REGISTER_INFO("Allocated {} for {} type in {}",
    //               element_size,
    //               demangle(typeid(T).name()),
    //               aligned_address);
    return static_cast<T*>(aligned_address);
  }

  template <typename T>
  [[nodiscard]] T* allocate_array(T& t) {
    const auto allocated_memory = allocate<T>();
    std::uninitialized_move(std::begin(t), std::end(t), allocated_memory);
    return allocated_memory;
  }

  template <typename T, typename... Args>
    requires(std::is_constructible_v<T, Args...> || is_constructible<T, Args...>)
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

struct allocator_emplacer_helper {
  template <typename T, typename... Args>
  static T* emplace(arena& alloc, Args&&... args) {
    return alloc.template emplace<T>(std::forward<Args>(args)...);
  }
};

template <typename T>
allocator<T>::allocator() noexcept
    : m_region(arena::instance()) {}

template <typename T>
template <typename U>
allocator<T>::allocator(const allocator<U>& other) noexcept
    : m_region(other.m_region) {}

template <typename T>
T* allocator<T>::allocate(std::size_t n) {
  return static_cast<T*>(m_region.allocate<T>(sizeof(T) * n));
}
} // namespace cmm::memory
