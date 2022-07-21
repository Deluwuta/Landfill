#!/bin/sh

. ~/.config/suckless/dwmblocks/scripts/colors

#dte="$(date +"%d/%m/%Y %I:%M%p ")"
dte="$(date +"%A %d, %k:%M%p ")"
echo -e "^c$datecol^ $dte"
