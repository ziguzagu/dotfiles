## -*- mode: shell-script; -*-

export LANG=en_US.UTF-8
export TERMINFO=$HOME/.terminfo

########################################
## PATH
########################################

export PATH="/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin"
export MANPATH="/usr/local/share/man:/usr/share/man"

## using coreutils on mac installed by homebrew
if which brew > /dev/null; then
    PATH="$(brew --prefix coreutils)/libexec/gnubin:$PATH"
    MANPATH="$(brew --prefix coreutils)/libexec/gnuman:$MANPATH"
fi

PATH="$HOME/bin:$PATH"

export GOPATH=$HOME

########################################
## Editor and Pager
########################################

# -Sb1     - bright white (foreground)
# -Sh1;33  - bright yello
# -Su4;36  - cyan with underline
export LV="-c -Ou8 -Sb1 -Sh1;33 -Su4;36 -Ss0;37;44"

export LESS="-q -g -R -j 10 -M"
export LESSCHARSET=utf-8
export LESS_TERMCAP_mb=$'\E[01;33m'  # begin blinking
export LESS_TERMCAP_md=$'\E[01;33m'  # begin bold (bold, bright yellow)
export LESS_TERMCAP_me=$'\E[0m'      # end mode
export LESS_TERMCAP_se=$'\E[0m'      # end standout-mode
export LESS_TERMCAP_so=$'\E[0;37;44m' # begin standout-mode (white on blue)
export LESS_TERMCAP_ue=$'\E[0m'      # end underline
export LESS_TERMCAP_us=$'\E[04;36m'  # begin underline - (underline, cyan)

export PAGER=less

export EDITOR="emacsclient -t"
export ALTERNATE_EDITOR=vi

########################################
## Misc
########################################

## colorized grep
export GREP_COLORS="ms=04;31:mc=01;33:sl=:cx=:fn=33:ln=33:bn=33:se=01;30"
