#include <algorithm>
#include <type_traits>
struct mytype {
  operator std::size_t() const { return 1; }
};
int a(mytype&& b) {}
int main() {
  const mytype t;
  a(std::move(t));
}
