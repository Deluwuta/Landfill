#!/bin/bash

# Kill all bar stances
killall -q polybar

# Wait til the process has been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch polybar using xmonad's config
polybar -c $HOME/.config/xmonad/polybar/config.ini &
