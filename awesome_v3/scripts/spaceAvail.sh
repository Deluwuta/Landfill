#!/bin/bash

echo `df -h | head -n4 | tail -n1 | awk '{print $4}'`
