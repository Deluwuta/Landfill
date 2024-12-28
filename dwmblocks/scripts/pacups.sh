#!/bin/sh

. ~/.config/suckless/dwmblocks/scripts/colors

cupd=$(checkupdates | wc -l)
echo "^c$pacmancol^ $cupd updates"
