#!/bin/sh

redshift -P -O 3000 &

feh --bg-fill "$HOME/Pictures/backgrounds/thevoid.png" &

/usr/bin/emacs --daemon &
# openrazer-daemon -r &
/usr/lib/polkit-kde-authentication-agent-1 &
