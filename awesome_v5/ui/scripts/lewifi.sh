#!/bin/sh
#
# . ~/.config/suckless/dwmblocks/scripts/colors
#
# Script que detecta si hay wifi y, en el caso de haberla, muestra si es ethernet o wireless
# Pa detectar si hay conexion
# cat /sys/class/net/(w*)enp0s3/operstate
# Devuelve 'up' si esta conectado, 'down' si no
#
# Pa dectectar si es ethernet o wireless
# tail -n+3 /proc/net/wireless
# Si no escupe nada, es ethernet, si escupe algo, es wireless
#
# En caso de que sea wireless, hay que comprobar la potencia de la conexion
#

# Comprobamos si hay o no conexion
connect="$(cat /sys/class/net/wlan0/operstate)"

if [[ $connect == "up" ]]; then
	typeCon="$(tail -n+3 /proc/net/wireless)" # We take the third line
	if [[ "$(echo $typeCon | wc -c)" == 1 ]]; then # If it is empty
		echo "🌐 ^c$wificol^Connected"
	else
		echo "^c$wificol^ Connected"
	fi
else
	echo "❌^c$wificol^Disconnected"
fi
