## -*- mode: shell-script; -*-

## alias
alias ls="ls -F --color=auto"
alias l="ls -lh"
alias ll="ls -Alh"
alias sc="screen -xRU"
alias gv="grep -v .svn"
alias lv="lv -c -Ou8"
alias less="less -gj10"
alias vs="svn status -u"
alias lc="tr '[:upper:]' '[:lower:]'"
alias uc="tr '[:lower:]' '[:upper:]'"

## prompt
PS1="\[\033[00;36m\][\u@\h:\w]\[\033[00m\]\\$ "

## sharing history on multiple session
function share_history {
    history -a
    history -c
    history -r
}
PROMPT_COMMAND='share_history'
shopt -u histappend
export HISTCONTROL=ignoredups
export HISTSIZE=10000

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profiles
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    source /etc/bash_completion
fi

if [ -f ~/perl5/perlbrew/etc/bashrc ]; then
    source ~/perl5/perlbrew/etc/bashrc
fi

## at sixapart
if [ -f /usr/local/6a/etc/bashrc ]; then
    source /usr/local/6a/etc/bashrc
fi
