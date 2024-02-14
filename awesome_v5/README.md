# Intento número 5
Sí, por desgracia este es el quinto intento de hacer un "AwesomeWM" que no me dé asco

## DEPENDENCIAS
+ amixer: Controlador de audio
+ brightnessctl: Para controlar el brillo de la pantalla
+ acpi: Necesario para el widget de la batería
+ nmcli: Se usa para detectar la conexión a internet 

## TODO
+ [X] Minimal battery widget (Necesito revisarlo)
+ [X] Minimal internet widget (Va de aquella manera)
+ [ ] Brightness and volume widgets
+ [ ] Prettier tags
+ [ ] Modularizar el código (Avances)

### 09/12/2023
+ Se ha incluido una taskbar, ahora la build posee 2 wibars
+ Se ha modularizado mínimamente el código, separando la funcionalidad de las wibar a sus respectivos ficheros

### 11/12/2023
+ Inicio de la creación de un widget que devuelva el estado de la conexión a internet

### 24/01/2024
+ Se logró hacer el widget que de muestra la conexión que se posee tomando además dos parámetros, uno para la interfaz ethernet y otro para la de wifi.
+ Se hacen uso de llamadas asíncronas (debido a que io.popen es peligroso), pero eso hace que crear un widget como el que yo quiero posea limitaciones al necesitar comparar los valores obtenido de la propia llamada asíncrona.
+ Se debe revisar y mejorar para poder usar dos timers y mejorar aún más la eficiencia, pero de momento logra su cometido (En VM con ethernet).
+ También fue eliminada la segunda wibar (en algún momento anterior a este), puede que vuelva, aunque ahora busco un enfoque más funcional y "minimal". "Primero que funcione todo, luego se hace bonito".
