#!/bin/bash

. ~/.config/suckless/dwmblocks/scripts/colors

avail="$(df -h | awk 'NR==4 { print $4 }')"
echo "^c$diskcol^ $avail"
