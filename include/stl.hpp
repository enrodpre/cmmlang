#pragma once

#include <optional>
#include <string>
#include <unordered_map>
#include <vector>
namespace cmm {

template <typename T>
struct default_singleton {
  default_singleton(const default_singleton&)            = delete;
  default_singleton& operator=(const default_singleton&) = delete;
  default_singleton(default_singleton&&)                 = delete;
  default_singleton& operator=(default_singleton&&)      = delete;

  static T& instance() {
    static T m_instance;
    return m_instance;
  }

protected:
  default_singleton()  = default;
  ~default_singleton() = default;
};

template <class Range, class T>
concept is_range_assignable = std::ranges::input_range<Range> &&
                              std::convertible_to<std::ranges::range_reference_t<Range>, T>;

template <typename T>
class stack {
public:
  using value_type                   = T;
  using pointer_type                 = T*;
  using container_type               = std::vector<T>;
  using const_pointer_type           = const T*;
  using reference_type               = T&;
  using const_reference_type         = const T&;
  using iterator                     = std::vector<T>::reverse_iterator;
  using reverse_iterator             = std::vector<T>::iterator;
  using const_iterator               = std::vector<T>::const_reverse_iterator;
  using const_reverse_iterator       = std::vector<T>::const_iterator;

  stack()                            = default;
  ~stack()                           = default;
  stack(const stack&)                = default;
  stack(stack&&) noexcept            = default;
  stack& operator=(const stack&)     = default;
  stack& operator=(stack&&) noexcept = default;

  stack(const container_type&);
  stack(container_type&&);
  template <is_range_assignable<T> Range>
  stack(Range&&);

  bool operator==(const stack& other) const noexcept;
  bool operator!=(const stack& other) const noexcept;

  iterator begin() noexcept;
  iterator end() noexcept;
  [[nodiscard]] const_iterator begin() const noexcept;
  [[nodiscard]] const_iterator end() const noexcept;
  [[nodiscard]] const_iterator cbegin() const noexcept;
  [[nodiscard]] const_iterator cend() const noexcept;
  [[nodiscard]] const_reverse_iterator crbegin() const noexcept;
  [[nodiscard]] const_reverse_iterator crend() const noexcept;

  T& top() noexcept;
  [[nodiscard]] const T& top() const noexcept;
  template <typename Func>
  bool contains(Func&& func) const;
  template <typename Func>
  const T& find(Func func) const;
  template <typename Func>
  std::optional<size_t> find_position(Func func) const;
  template <typename Func>
  const_iterator find_all(Func func) const;
  template <typename Func>
  size_t count(Func func) const;
  template <typename... Args>
    requires std::is_constructible_v<T, Args...>
  void emplace(Args&&... args);
  void push(const T& t);
  void push(T&& t) noexcept;
  void pop();
  auto pop_value();
  void clear() noexcept { return m_data.clear(); }
  void swap(stack& other) noexcept;
  [[nodiscard]] constexpr size_t size() const noexcept { return m_data.size(); }
  [[nodiscard]] constexpr size_t max_size() const noexcept { return m_data.max_size(); }
  [[nodiscard]] constexpr bool empty() const noexcept { return m_data.empty(); }

protected:
  container_type m_data;
};
template <typename K, typename V>
class hashmap {
public:
  using key_type       = K;
  using value_type     = V;
  using container_type = std::unordered_map<std::string, value_type>;
  hashmap()            = default;

  const container_type& data() const { return m_store; }

  [[nodiscard]] bool contains(const key_type&) const;
  [[nodiscard]] size_t size() const noexcept;
  [[nodiscard]] container_type::const_iterator begin() const { return m_store.begin(); }
  [[nodiscard]] container_type::const_iterator end() const { return m_store.end(); }
  [[nodiscard]] container_type::const_iterator cbegin() const { return m_store.begin(); }
  [[nodiscard]] container_type::const_iterator cend() const { return m_store.cend(); }

  value_type& at(const key_type&);
  const value_type& at(const key_type&) const;
  void insert(const key_type&, value_type&&);
  void insert(const key_type&, const value_type&);
  value_type& operator[](const key_type&);
  const value_type& operator[](const key_type&) const;
  template <typename... Args>
    requires std::is_constructible_v<V, Args...>
  value_type& emplace(key_type, Args&&...);
  void clear();

private:
  container_type m_store;
};

} // namespace cmm

#include "stl.inl"
