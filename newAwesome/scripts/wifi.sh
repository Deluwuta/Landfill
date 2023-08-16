#!/bin/bash

conn="$(cat /sys/class/net/enp0s3/operstate)"

if [[ $conn == "up" ]]; then
    echo "Ethernet"
else
    echo "No wifi"
fi
