## $Id$

#### alias
alias ls="ls -F --color=auto"
alias l="ls -l"
alias ll="ls -al"
alias sc="screen -xRU"
alias gv="grep -v .svn"
alias lv="lv -c -Ou8"
alias less="less -gj10"
alias vs="svn status -u"
alias lc="tr '[:upper:]' '[:lower:]'"
alias uc="tr '[:lower:]' '[:upper:]'"

#### prompt
PS1="\[\033[00;36m\][\h:\w]\[\033[00m\]\\$ "

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profiles
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi
