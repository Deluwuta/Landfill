#!/usr/bin/sh

# Volume pipe creation
_volume_pipe1=/tmp/.volume-pipe1
_volume_pipe2=/tmp/.volume-pipe2

_bright_pipe1=/tmp/.bright-pipe1
_bright_pipe2=/tmp/.bright-pipe2

[[ -S $_volume_pipe1 ]] || mkfifo $_volume_pipe1
[[ -S $_volume_pipe2 ]] || mkfifo $_volume_pipe2

[[ -S $_bright_pipe1 ]] || mkfifo $_bright_pipe1
[[ -S $_bright_pipe2 ]] || mkfifo $_bright_pipe2

sleep 5

vol=0
vol=$(amixer sget Master | grep -o -m 1 '[[:digit:]]*%' | tr -d '%')
echo ${vol}% | tee /tmp/.volume-pipe1
echo ${vol}% | tee /tmp/.volume-pipe2

bri=0
bri=$(brightnessctl i | awk 'NR==2 { print $4 }' | sed -r 's/[()]//g')
echo ${bri} | tee /tmp/.bright-pipe1
echo ${bri} | tee /tmp/.bright-pipe2
