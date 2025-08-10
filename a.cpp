#include <iostream>
int main() {
  int x = 1;
  int y = ++x + 12 * x--;
  std::cout << y;
}
