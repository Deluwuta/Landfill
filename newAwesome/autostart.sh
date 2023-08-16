#!/bin/bash

# redshift -P -O 3300 &
# picom &

feh --bg-fill $HOME/Pictures/wallpaper1.png &
# feh --bg-fill $HOME/Pictures/wallpaper2.png &
/usr/bin/emacs --daemon &

# Notifications kek
# killall -q dunst
# while pgrep -u $UID -x dunst >/dev/null; do
#  sleep 1
# done
# dunst &

setxkbmap us -variant altgr-intl -option nodeadkeys
