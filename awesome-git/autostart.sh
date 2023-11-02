#!/bin/sh

redshift -P -O 3000 &

xwallpaper --zoom "$HOME/Pictures/backgrounds/nixos2.png" &

/usr/bin/emacs --daemon &
# openrazer-daemon -r &
/usr/lib/polkit-kde-authentication-agent-1 &
