#!/usr/bin/env bash

## for mac os
if which vm_stat > /dev/null 2>&1; then
    _vmstat=$(vm_stat) || return 1
    _pages_free=$(echo "$_vmstat" | awk '/Pages free/ {print $3}' | tr -d '.')
    _pages_active=$(echo "$_vmstat" | awk '/Pages active/ {print $3}' | tr -d '.')
    _pages_inactive=$(echo "$_vmstat" | awk '/Pages inactive/ {print $3}' | tr -d '.')
    _pages_speculative=$(echo "$_vmstat" | awk '/Pages speculative/ {print $3}' | tr -d '.')
    _pages_wired=$(echo "$_vmstat" | awk '/Pages wired down/ {print $4}' | tr -d '.')

    _free=$((($_pages_free + $_pages_speculative) * 4096 / 1024))
    _used=$((($_pages_active + $_pages_inactive + $_pages_wired) * 4096 / 1024))
    _total=$(($_free + $_used))
    
    echo "$(($_free * 100 / $_total))%($(($_free / 1024))MB)"
else
    free -m | awk '/\-\/\+ / {print $4"MB"}'
fi
