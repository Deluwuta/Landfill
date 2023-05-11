#!/bin/bash

redshift -P -O 3600 &

feh --bg-fill $HOME/Pictures/Backgrounds/Coso_raro_pero_cuco.png
# Notifications kek
killall -q dunst
while pgrep -u $UID -x dunst >/dev/null; do
  sleep 1
done
dunst &

# THE BAR
dwmblocks &
