#!/bin/sh

xrandr -s 1920x1080
xwallpaper --zoom "$HOME/Pictures/baelz-wall1.jpg" &
/usr/bin/emacs --daemon &

/usr/lib/polkit-kde-authentication-agent-1 &
