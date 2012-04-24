#!/bin/sh
if [ -x "/usr/bin/pmset" ]; then
    /usr/bin/pmset -g ps | grep InternalBattery | awk '{print $2 "(" $4 ")"}' | sed 's/;//'
fi
