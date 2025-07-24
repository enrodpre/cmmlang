#include <iostream>
#include <memory>
int main() {
  std::cout << sizeof(std::shared_ptr<int>) << std::endl;
}
