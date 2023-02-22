#!/bin/bash

TEMP=/tmp/current_wall

files=($HOME/Pictures/Backgrounds/*)
file_num=`ls $HOME/Pictures/Backgrounds/ | wc -l` 
random_num=$(( $RANDOM % $file_num ))
random_num=`expr $random_num + 1`

first_start () {
  swww kill
  while pgrep -u $UID -x swww >/dev/null; do
    sleep 1
  done
  swww init
  echo $random_num > $TEMP
  swww img "${files[$random_num]}"
}

change () {
  index=$(cat $TEMP)
  index=$((index+$1))
  if [ $index -ge ${#files[@]} ]; then
    index=0
  fi
  echo $index > $TEMP
  swww img "${files[$index]}" --transition-fps 60 --transition-type grow --transition-pos 0.925,0.977 --transition-duration 2
  exit 0
}

if [[ "$1" == "first" ]]; then
  first_start 
elif [[ "$1" == "change" ]]; then
  change $2
fi
