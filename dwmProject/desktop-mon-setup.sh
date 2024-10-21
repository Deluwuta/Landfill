#!/usr/bin/env bash

# Use xrandr to check the monitors connected and its names
primary="DP-2"
secondary="HDMI-1"
position="right-of" # Options: right-of, left-of, above, below

xrandr --output "$primary" --primary
xrandr --output "$secondary" --auto --"$position" "$primary"
