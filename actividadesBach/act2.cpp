#include <iostream>

using namespace std;

int main(int argc, char *argv[]) {
  int ancho = 0, alto = 0;
  cout << "Dime el ancho: ";
  cin >> ancho;
  cout << "Dime el alto: ";
  cin >> alto;

  for (int i = 0; i < alto; i++) {
    for (int k = 0; k < ancho; k++)
      cout << "*";
    cout << endl;
  }
  return 0;
}
