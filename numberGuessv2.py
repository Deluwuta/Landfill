#!/usr/bin/env python3

import random

def otorgarPista(rand, x):
    if rand > x:
        print("El valor introducido es *menor* :^)")
    else:
        print("El valor introducido es *mayor* :^)")


# Modulo principal del jueguito (Si esto fueran clases seria una encapsulacion de la ostia ngl)
def juegarRealKEK(suelo, techo, intentos, dificultad, name):
    print("Dificultad elegida: " + dificultad + ".\nTienes " + str(intentos) + " intentos para elegir un numero entre " + str(suelo) + " y " + str(techo) + ".\nSi llegan a 0, pierdes. Facil no?")
    rand = random.randint(suelo, techo)
    win = False
    # print(rand) # Pa pruebas

    while intentos != 0 and not win:
        eleccion = input("Pruebe suerte: ")
        eleccion = int(eleccion)
        if rand == eleccion:
            print("Su respuesta es CORRECTA. Enhorabuena " + str(name) + ", le sobraron " + str(intentos) + " intentos.")
            win = True
        else:
            intentos -= 1
            if intentos == 0:
                print("No le quedan mas intentos. Ya me joderia ser tan putisimo inutil.")
            else:
                print("Su respuesta es INCORRECTA. Le quedan " + str(intentos) + " intentos. Pruebe de nuevo.")
                otorgarPista(rand, eleccion)


# Para ajustar la dificultad del juego.
def juegar(modo, name): # Esta escrito a posta.
    match modo:
        case 1:
            juegarRealKEK(0, 5, 5, "Easy", name)
        case 2:
            juegarRealKEK(0, 10, 8, "Normal", name)
        case 3:
            juegarRealKEK(0, 50, 10, "Hard", name)


def elegirDificultad(): # Para comprobar que el numero introducido no se sale del rango establecido
    print("Introduzca la dificultad del juego.\n1. Easy\n2. Normal\n3. Hard")
    valido = False;
    while not valido:
        try:
            dificultad = input()
            dificultad = int(dificultad)
            if dificultad > 3 or dificultad <= 0:
                print("Valor no permitido. Introduzcalo de nuevo.")
            else:
                valido = True
        except:
            print("Valor no valido. Introduzcalo de nuevo.")
    return dificultad


# Funcion reuseless. Me flipa tener el main ultralimpio.
def introduccion():
    print("Proyecto para practicar en Python. Number guessing game.")
    return input("Su nombre: ")


def main():
    jugador = introduccion()
    modo = elegirDificultad()
    juegar(modo, jugador)


if __name__ == "__main__":
    main()
