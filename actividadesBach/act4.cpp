#include <iostream>
#include <vector>

using namespace std;

void act4Version(int number, vector<int> v) {
  cout << endl
       << "Ahora vas a ir metiendo numeros y te voy a decir en que posicion "
          "del array estan :)"
       << endl;
  do {
    cout << "Dime un numero (0 para salir): ";
    cin >> number;
    if (number != 0) {
      for (int x = 0; x < v.size(); x++)
        if (v[x] == number)
          cout << "Esta en la posicion: " << x << endl;
    }
  } while (number != 0);
}

void act5Version(int number, vector<int> v) {
  cout << endl
       << "Ahora vas a ir metiendo numeros y te voy a decir si existen o no :)"
       << endl;
  do {
    cout << "Dime un numero (0 para salir): ";
    cin >> number;
    if (number != 0) {
      for (int x = 0; x < v.size(); x++) {
        if (v[x] == number) {
          cout << "Encontrado" << endl;
          break;
        }
      }
    }
  } while (number != 0);
}

int main(int argc, char *argv[]) {
  vector<int> v(10, 0);
  int number, suma = 0;
  for (int i = 0; i < 10; i++) {
    cout << "Mete un numero: ";
    cin >> number;
    v[i] = number;
  }

  int version;
  cout << "Elige version del problema crack XD (1, 2): ";
  cin >> version;

  if (version == 1)
    act4Version(number, v);
  else
    act5Version(number, v);

  cout << "Hasta luego!" << endl;
  return 0;
}
