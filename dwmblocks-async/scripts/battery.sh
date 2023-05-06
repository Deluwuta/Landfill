#!/bin/sh

# . ~/.config/suckless/dwmblocks/scripts/colors

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
        $(($remaining > 89))) echo " $remaining%" ;;
        $(($remaining > 59))) echo " $remaining%" ;;
        $(($remaining > 29))) echo " $remaining%" ;;
        $(($remaining > 19))) echo " $remaining%" ;;
        $(($remaining < 19))) echo " $remaining%" ;;
    esac

elif [[ $mode == "Charging" || $mode == "Not charging" ]]; then
    echo " $remaining%"
fi
