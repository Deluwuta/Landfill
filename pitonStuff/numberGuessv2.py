#!/usr/bin/env python3

import random

def otorgarPista(rand, x):
    if rand > x:
        print("El valor que tú has introducido es *menor* :^)")
    else:
        print("El valor que tú has introducido es *mayor* :^)")


# Módulo principal del juegiño (Si esto fueran clases sería una encapsulacion de la hostia, ngl)
def juegarReal(suelo, techo, intentos, dificultad, name):
    print("Dificultad elegida: " + dificultad + ".\nTienes " + str(intentos) + " intentos para elegir un número entre " + str(suelo) + " y " + str(techo) + ".\nSi llegan a 0, pierdes. Fácil, ¿no?")
    rand = random.randint(suelo, techo)
    win = False
    # print(rand) # Pa' pruebas

    while intentos != 0 and not win:
        eleccion = input("\nPruebe suerte: ")
        # Comprobamos que el input ha sido un número y no cualquier otra cosa
        try:
            eleccion = int(eleccion)
        except:
            intentos -= 1
            print("Mete un número ni que sea no? '-'. Por especial te quito un intento. Te quedan " + str(intentos))
            continue

        # Confirmamos que es un número
        intentos -= 1
        # uwu
        if rand == eleccion:
            print("Tu respuesta es CORRECTA. Enhorabuena " + str(name) + ", sobraron " + str(intentos) + " intento(s).")
            win = True
        elif intentos == 0:
            print("No te quedan más intentos. Ya me jodería ser tan putísimo inútil.")
        else:
            print("Tu respuesta es INCORRECTA. Te quedan " + str(intentos) + " intentos. Prueba de nuevo.")
            otorgarPista(rand, eleccion)


# Para ajustar la dificultad del juego.
def juegar(modo, name): # Esta escrito aposta.
    if modo == 1:
      juegarReal(0, 5, 5, "Easy", name)
    elif modo == 2:
      juegarReal(0, 9, 7, "Normal", name)
    else: # Seguro es 3 si llega aquí
      juegarReal(0, 49, 10, "Hard", name) 


# Para ajustar la dificultad del juego but cooler.
def juegar2(modo):
  return {
    1 : [0, 5, 5, "Easy"],
    2 : [0, 9, 7, "Normal"],
    3 : [0, 49, 10, "Hard"],
  }.get(modo, [])


def elegirDificultad(): # Para comprobar que el número introducido no se sale del rango establecido
    print("Introduzca la dificultad del juego.\n1. Easy\n2. Normal\n3. Hard")
    valido = False;
    while not valido:
        try:
            dificultad = input()
            dificultad = int(dificultad)
            if dificultad > 3 or dificultad <= 0:
                print("Valor no permitido. Introdúzcalo de nuevo.")
            else:
                valido = True
        except:
            print("Valor no válido. Introdúzcalo de nuevo.")
    return dificultad


# Funcion reuseless. Me flipa tener el main ultralimpio.
def introduccion():
    print("Proyecto para practicar en Python. Number guessing game.")
    return input("Su nombre: ")


def main():
    jugador = introduccion()
    modo = elegirDificultad()
    #juegar(modo, jugador) # Como lo hacen las personas normales
    tupla = juegar2(modo) # Como lo hacen los auténticos psicópatas
    juegarReal(tupla[0], tupla[1], tupla[2], tupla[3], jugador)


if __name__ == "__main__":
    main()
