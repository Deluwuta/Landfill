#!/bin/sh
ker="$(uname -r | cut -d- -f1)"
echo -e "$ker"
