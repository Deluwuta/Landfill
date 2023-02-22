#!/bin/bash

# Russian roulette for Linux! :D
number=$(( $RANDOM%100 ))

if [[ $number == 0 ]]; then
  echo "'-'"
else
  /sbin/shutdown now
fi
