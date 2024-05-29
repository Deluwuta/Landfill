#!/bin/env bash

# Just a script to restart Waybar cuz
# killall -SIGUSR2 waybar does not work at all xD

killall -q waybar
while pgrep -u $UID -x waybar >/dev/null; do
  sleep 1
done
waybar -c $HOME/.config/waybar/config.jsonc -s $HOME/.config/waybar/style.css &
