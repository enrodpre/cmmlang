int main() {
  int t = 1;
  std::cout << t;
  {
    int t = 3;
    std::cout << t;
  }
  std::cout << t;
}
