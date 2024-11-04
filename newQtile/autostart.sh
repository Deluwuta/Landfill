#!/usr/bin/env bash

### VM Specifics ###
# Common things I have to autostart when using vm's
# xrandr -s 1920x1080
# sleep 0.5 # To ensure the wallpaper is displayed correctly
# setxkbmap us intl altGr dead keys
# setxkbmap es
# ###################

# My eyes are broken
redshift -P -O 3000 &
nm-applet &

# Wallpaper
WALLPAPER_DIR="$HOME/Pictures/wallpapers"
WALLPAPER_DEFAULT="$HOME/Pictures/wallpapers/fuwamoco2.jpg"

## I prefer xwallpaper, but for some reason Fedora does not have it
# feh --bg-fill --randimize "$WALLPAPER_DIR"
feh --bg-fill "$WALLPAPER_DEFAULT"

# Now I use more than one monitor
exec "$HOME/.config/scripts/desktop-mon-setup.sh"

# THA BAR (dwmblocks version)
# killall -q dwmblocks
# while pgrep -u $UID -x dwmblocks >/dev/null; do
#     sleep 0.2
# done
# dwmblocks &

# THA BAR (polybar version)
# exec "$HOME/.config/polybar/launch.sh"
