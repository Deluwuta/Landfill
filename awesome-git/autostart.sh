#!/bin/sh

xrandr -s 1920x1080
xwallpaper --zoom "$HOME/Pictures/cozy_room.png" &
/usr/bin/emacs --daemon &

/usr/lib/polkit-kde-authentication-agent-1 &
