#include <iostream>

int main(int argc, char *argv[]) {
  int number;
  std::cout << "Dime un numero: ";
  std::cin >> number;
  std::cout << "Sus divisores son: 1 ";

  for (int i = 2; i <= number / 2; i++) {
    if (number % i == 0)
      std::cout << i << " ";
  }
  std::cout << number << std::endl;
  return 0;
}
