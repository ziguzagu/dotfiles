## -*- mode: shell-script; -*-

## lv
# -Sb1     - bright white (foreground)
# -Sh1;31  - bright red
# -Su4;36  - cyan with underline
export LV="-c -Ou8 -Sb1 -Sh1;31 -Su4;36"

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
alias gv="grep -v .svn"
alias vs="svn status -u"
alias lc="tr '[:upper:]' '[:lower:]'"
alias uc="tr '[:lower:]' '[:upper:]'"
alias prove="prove -lv --timer"
alias grep="grep --binary-files=without-match --color=always"
alias g="git"

## prompt
PS1="\[\e[31m\][\u@\h \w]\[\e[00m\]\\$ "

## sharing history on multiple session
function share_history {
    history -a
    history -c
    history -r
}
PROMPT_COMMAND='share_history'
shopt -u histappend
export HISTCONTROL=erasedups
export HISTIGNORE=cd:history:ls:which
export HISTSIZE=10000

## dircolors
if which dircolors > /dev/null; then
    test -f ~/.dircolors && eval `dircolors ~/.dircolors`
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profiles
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    source /etc/bash_completion
fi

if [ -f ~/perl5/perlbrew/etc/bashrc ]; then
    source ~/perl5/perlbrew/etc/bashrc
elif [ -z "$PERL5LIB" ]; then
    eval `perl -Iperl5/lib/perl5 -Mlocal::lib 2>/dev/null`
fi