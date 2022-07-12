#!/bin/bash

# Kill bar stances
killall -q polybar

# Wait til the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch polybar using ~/.config/polybar/config.ini 
polybar &
