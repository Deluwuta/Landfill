#!/bin/bash

# Kill all bar stances
killall -q polybar

while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launchin
polybar -c $HOME/.config/polybar/config.ini &
