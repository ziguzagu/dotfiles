## -*- mode: shell-script; -*-

export LANGUAGE=en_US:en
export LANG=en_US.UTF-8

export EDITOR=vi

## the default umask is set in /etc/login.defs
umask 022

## include .bashrc if it exists
if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi
