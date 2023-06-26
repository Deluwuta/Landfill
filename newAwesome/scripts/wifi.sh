#!/bin/bash

conn="$(cat /sys/class/net/enp3s0/operstate)"

if [[ $conn == "up" ]]; then
    echo "Ethernet"
else
    echo "No wifi"
fi
