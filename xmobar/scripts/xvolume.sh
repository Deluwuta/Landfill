#!/usr/bin/bash

# return volume levels (0-100)
vol=$(amixer sget Master | grep -o -m 1 '[[:digit:]]*%' | tr -d '%')
echo ${vol}% | tee /tmp/.volume-pipe1
echo ${vol}% | tee /tmp/.volume-pipe2
