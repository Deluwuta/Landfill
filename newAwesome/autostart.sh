#!/bin/bash

redshift -P -O 3300 &

feh --bg-fill $HOME/Pictures/background/csm_power1.jpg

/usr/bin/emacs --daemon &

# Notifications kek
killall -q dunst
while pgrep -u $UID -x dunst >/dev/null; do
  sleep 1
done
dunst &
