#!/usr/bin/env python3
import random


print("Hola mi compa. Vamos a jugar a un jueguito asi tonto no te rayes.")
name = input("Antes que nada, dime tu nombre crack: ")

print("Hola " + name + " un placer.\nEl juego consiste en que tienes que adivinar un numero del 0 al 9. Sencillo, no?\nSi fallas te quedas sin pc, SUERTEEEE")

rand = random.randint(0, 9) # Genera un pseudorandom entre 0 y 9, ambos incluidos
valor = input("Prueba suerte: ")

if valor == "coffee": # Aqui valor es un string
    print("What... Okay, has ganado, me gusta el cafe '-'")

else:
    try:
        newValor = int(valor) # transformamos valor a un int
        if newValor == rand:  # comparamos los dos ints
            print("ACERTASTE, ENHORABUENA, una pena que aun asi te vayas a quedar sin pc, unlucky :^)")
        else    :
            print("F\nIniciando shutdown...")
    except:                   # Menejamos la posible excepcion
        print(" Hermano, mete un numero al menos no? '-'")
