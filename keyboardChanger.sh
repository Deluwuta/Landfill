#!/bin/bash

# Bash script que cambia el layout del teclado
# Utiliza la funcionalidad xbk-switch, la cual escupe el layout actual

DIR=/home/delta/.config/dunst/notif_scripts/

case `xkb-switch` in
    'es')
        setxkbmap us -variant altgr-intl -option nodeadkeys
        $DIR/notify-send.sh -u low -t 1500 --replace=555 "Layout changed to (cooler) us"
        ;;
    'us(altgr-intl)')
        setxkbmap es
        $DIR/notify-send.sh -u low -t 1500 --replace=555 "Layout changed to es"
        ;;
    *)
        setxkbmap us -variant altgr-intl -option nodeadkeys
        ;;
esac

