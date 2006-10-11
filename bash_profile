## $Id$

## languages and charsets
export LANGUAGE=ja_JP:ja:en_GB:en
export LC_ALL=C
export LANG=ja_JP.UTF-8

## the default umask is set in /etc/login.defs
umask 022

## include .bashrc if it exists
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

## set PATH so it includes user's private bin if it exists
if [ -d ~/bin ] ; then
    PATH=~/bin:"${PATH}"
fi
## do the same with MANPATH
if [ -d ~/man ]; then
    MANPATH=~/man${MANPATH:-:}
    export MANPATH
fi

#### history
export HISTCONTROL=ignoredups
export HISTSIZE=100000
