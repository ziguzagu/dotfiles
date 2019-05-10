## -*- mode: shell-script; -*-

## less
export LESS="-qgR -j10"
export LESSCHARSET=utf-8
export LESS_TERMCAP_mb=$'\E[01;31m'  # begin blinking
export LESS_TERMCAP_md=$'\E[01;31m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'      # end mode
export LESS_TERMCAP_se=$'\E[0m'      # end standout-mode
export LESS_TERMCAP_so=$'\E[7m'      # begin standout-mode
export LESS_TERMCAP_ue=$'\E[0m'      # end underline
export LESS_TERMCAP_us=$'\E[04;36m'  # begin underline

## page and editor
export PAGER=less
export GIT_PAGER="less -FX"
export EDITOR=vi

## alias
alias ls="ls -F --color=auto"
alias l="ls -lh"
alias ll="ls -Alh"
alias sc="screen -xRU"
alias lc="tr '[:upper:]' '[:lower:]'"
alias uc="tr '[:lower:]' '[:upper:]'"
alias prove="prove -lv --timer"
alias grep="grep --binary-files=without-match --color=always"
alias g="git"

## prompt
PS1="\[\e[31m\][\u@\h \w]\[\e[00m\]\\$ "

## sharing history on multiple session
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"
shopt -s histappend
export HISTCONTROL=erasedups
export HISTIGNORE=cd:history:ls:which
export HISTSIZE=10000

## dircolors
if which dircolors > /dev/null; then
    test -f ~/.dircolors && eval "$(dircolors ~/.dircolors)"
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profiles
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    source /etc/bash_completion
fi
