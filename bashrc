## $Id$

#### alias
alias ls="ls -F"
alias l="ls -Al"
alias ll="ls -al"
alias sc="screen -xRU"
alias gv="grep -v .svn"
alias lv="lv -c"
alias vs="svn status -u"

#### prompt
PS1="(J\(B[(J\(B033[00;36m(J\(B][(J\(Bw](J\(B[(J\(B033[00m(J\(B](J\(B$ "

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profiles
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi
