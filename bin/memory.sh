#!/usr/bin/env bash

## for mac os
if which vm_stat > /dev/null 2>&1; then
    vmstat=$(vm_stat) || return 1
    pages_free=$(echo "$vmstat" | awk '/Pages free/ {print $3}' | tr -d '.')
    pages_active=$(echo "$vmstat" | awk '/Pages active/ {print $3}' | tr -d '.')
    pages_inactive=$(echo "$vmstat" | awk '/Pages inactive/ {print $3}' | tr -d '.')
    pages_speculative=$(echo "$vmstat" | awk '/Pages speculative/ {print $3}' | tr -d '.')
    pages_wired=$(echo "$vmstat" | awk '/Pages wired down/ {print $4}' | tr -d '.')

    free=$((($pages_free + $pages_speculative) * 4096 / 1024))
    used=$((($pages_active + $pages_inactive + $pages_wired) * 4096 / 1024))
    total=$(($free + $used))
    
    echo "$(($free * 100 / $total))%($(($free / 1024))MB)"
else
    free -m | awk '/\-\/\+ / {print $4"MB"}'
fi
