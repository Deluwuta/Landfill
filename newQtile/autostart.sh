#!/bin/bash

redshift -P -O 3300 &

feh --bg-fill $HOME/Pictures/background/bg.png

/usr/bin/emacs --daemon &

# Notifications kek
killall -q dunst
while pgrep -u $UID -x dunst >/dev/null; do
  sleep 1
done
dunst &
