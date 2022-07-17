#!/bin/bash
avail="$(df -h | awk 'NR==4 { print $4 }')"
echo "$avail"
