## -*- mode: shell-script; -*-

export LANG=en_US.UTF-8
export TERMINFO=$HOME/.terminfo

########################################
## PATH
########################################

export PATH="$HOME/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin"
export MANPATH="/usr/local/share/man:/usr/share/man"

export GOPATH=$HOME

########################################
## Editor and Pager
########################################

export PAGER=less

export EDITOR="emacsclient -t"
export ALTERNATE_EDITOR=vi
