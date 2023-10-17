#!/usr/bin/bash

bri=$(brightnessctl i | awk 'NR==2 { print $4 }' | sed -r 's/[()]//g')
echo ${bri} | tee /tmp/.bright-pipe1
echo ${bri} | tee /tmp/.bright-pipe2
