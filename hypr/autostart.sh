#!/bin/bash

config=$HOME/.config/hypr

# Nightlight cuz my eyes hurts :')
gammastep -PO 4000 &

# Notifications kek
killall -q dunst
while pgrep -u $UID -x dunst >/dev/null; do
  sleep 1
done
dunst &

# Killin waybar via script
$HOME/.config/hypr/restartinWaybar.sh

# Swww cuz I want wallpapers
swww init 
swww img $HOME/Pictures/Backgrounds/voidlinax.jpg

dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP &
