#!/bin/bash

# My eyes hurst broh
redshift -P -O 4600 &

feh --bg-fill $HOME/Pictures/Backgrounds/arknightsFeater2.png

# Notifications kek
killall -q dunst
while pgrep -u $UID -x dunst >/dev/null; do
  sleep 1
done
dunst &

# THE BAR
dwmblocks &
