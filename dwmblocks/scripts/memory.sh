#!/bin/sh

. ~/.config/suckless/dwmblocks/scripts/colors

mem="$(free -h | awk '/^Mem:/ {print $3 "/" $2}')"
echo -e "^c$memcol^ $mem"
