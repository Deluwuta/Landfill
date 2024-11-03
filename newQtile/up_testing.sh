#!/usr/bin/env bash

# Script to test updating text widgets with Qtile API


if [ $# -ne 1 ]; then
    echo "`basename $0`: I need one parameter bruh" 1>&2
    exit 1
fi

if [ ! -f "./notread.txt" ]; then
    echo -n "5" > "notread.txt"
    echo "Fichero notread creado"
fi

number=$(head -n 1 "./notread.txt")

if [ "$1" == "up" ]; then
    echo -n "$(( $number + 10 ))" > "notread.txt"

elif [ "$1" == "down" ]; then
    aux=$(( $number - 10 ))
    if (( $aux >= 0 )); then
        echo -n $aux > "notread.txt"
    else
        echo -n 0 > "notread.txt"
    fi

else
    echo "Not a valid argument lol"
fi

echo $(cat "./notread.txt")
