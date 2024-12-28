#!/bin/sh

. ~/.config/suckless/dwmblocks/scripts/colors

# Full (100-90)   (Light green)
# High (89-60)   (Green)
# Half (59~30)   (Bold green)
# Low (29~20)   (Orange)
# Danger (19~0)  (Red)
# Charging  (Yellow)

remaining="$(cat /sys/class/power_supply/BAT0/capacity)"
mode="$(cat /sys/class/power_supply/BAT0/status)"

if [[ $mode == "Discharging" ]]; then
    case 1 in
        $(($remaining > 89))) echo "^c$batfull^  $remaining%" ;;
        $(($remaining > 59))) echo "^c$bathigh^  $remaining%" ;;
        $(($remaining > 29))) echo "^c$bathalf^  $remaining%" ;;
        $(($remaining > 19))) echo "^c$batlow^  $remaining%" ;;
        $(($remaining < 19))) echo "^c$batdanger^ $remaining%" ;;
    esac

elif [[ $mode == "Charging" ]]; then
    echo "^c$batchar^ $remaining%"
fi
