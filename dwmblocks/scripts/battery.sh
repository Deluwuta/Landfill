#!/usr/bin/env sh

VAL=$(acpi -b)

if [ -n "${VAL}" ]; then
    echo "$VAL"
fi
