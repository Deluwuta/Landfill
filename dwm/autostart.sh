#!/usr/bin/env bash

# Idk what this if for (dwm)
xset s off
xset s noblank
xset -dpms

# VM's
xrandr -s 1920x1080
sleep 0.2
setxbkmap us intl altGr

redshift -P -O 3000 &
nm-applet &

feh --bg-fill $HOME/Pictures/irene_arknights.jpg
# wal -R

# Launch dwmblocks
killall -q dwmblocks
while pgrep -u $UID -x dwmblocks >/dev/null; do 
    sleep 0.2
done
dwmblocks &
