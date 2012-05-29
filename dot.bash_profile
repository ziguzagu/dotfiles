## languages and charsets
export LANGUAGE=en_US
export LC_ALL=C
export LANG=en_US.UTF-8

## the default umask is set in /etc/login.defs
umask 022

## include .bashrc if it exists
if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi
