#!/bin/sh

. ~/.config/suckless/dwmblocks/scripts/colors

ker="$(uname -r | cut -d- -f1)"
echo -e "^c$kernelcol^ $ker"
#echo -e "^c#A60300^$ker"
