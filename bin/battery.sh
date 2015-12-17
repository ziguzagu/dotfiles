#!/bin/sh
if [ -x "/usr/bin/pmset" ]; then
    /usr/bin/pmset -g ps | grep InternalBattery | sed 's/[;()]//g' | perl -anE 'say "$F[1]($F[3])"'
else
    echo -n ":D"
fi
