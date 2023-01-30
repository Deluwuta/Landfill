#include <iostream>

using namespace std;

int main(int argc, char *argv[]) {
  int number, suma = 0;
  for (int i = 0; i < 10; i++) {
    cout << "Dame un numero: ";
    cin >> number;
    suma += number;
  }
  cout << "La suma de to eso es: " << suma << endl;
  return 0;
}
