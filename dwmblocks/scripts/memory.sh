#!/usr/bin/env sh

VAL=$(free -h | awk '/^Mem/ { print $3 "/" $2 }' | sed s/i//g)
echo "  $VAL"
