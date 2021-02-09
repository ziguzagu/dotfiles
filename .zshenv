# -*- mode: shell-script; -*-

export LANG=en_US.UTF-8
export TERMINFO=~/.local/share/terminfo

export PATH=$HOME/bin:$PATH
export GOPATH=$HOME

export PAGER=less

export EDITOR="emacsclient -t"
export ALTERNATE_EDITOR=vi

export GPG_TTY=$(tty)

eval $(/usr/local/bin/brew shellenv)
