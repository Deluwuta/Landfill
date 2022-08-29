#!/bin/bash 
# 🔈🔉
. ~/.config/suckless/dwmblocks/scripts/colors

vol="$(amixer get Master | tail -n1 | sed -r 's/.*\[(.*)%\].*/\1/')"
echo "^c$volcol^🔊 ${vol}%"
