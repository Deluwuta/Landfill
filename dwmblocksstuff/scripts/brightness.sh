#!/bin/sh

. ~/.config/suckless/dwmblocks/scripts/colors

current="$(brightnessctl i | awk 'NR==2 { print $4 }' | sed -r 's/[()]//g')"
echo "^c$brightcol^ $current"
