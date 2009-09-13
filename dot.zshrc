## -*- mode: shell-script; -*-
## $Id$

## completion
autoload -U compinit
compinit
# case insensitive
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'


## alias
setopt complete_aliases
alias ec="emacsclient -n"
if [ `uname` = "Linux" ]; then
    alias ls="ls -F --color=auto"
else
    alias ls="ls -FG"
fi
alias l="ls -l"
alias ll="ls -al"
alias sc="screen -xRU"
alias gv="grep -v .svn"
alias lv="lv -c -Ou8"
alias less="less -gj10"
alias vs="svn st -u"
alias vd="svn di"
alias vl="svn log --stop-on-copy"
alias gs="git-svn"
alias push="svk push --verbatim"
alias us="TZ=US/Pacific date"
alias lc="tr '[:upper:]' '[:lower:]'"
alias uc="tr '[:lower:]' '[:upper:]'"
alias htdate="date '+%a, %d %b %Y %X %Z'"
alias prove="prove -lv"
alias ack="ack --color"
alias grep="grep --binary-files=without-match --color=always"
alias t="todo.sh -d $HOME/.todo"
## global alias
alias -g M="| more"
alias -g L="| lv"
alias -g G="| grep"
alias -g GV="| grep -v"

## sets keybind like emacs
bindkey -e

## directory handlings
setopt auto_cd
setopt auto_pushd
setopt auto_name_dirs
setopt auto_remove_slash
setopt extended_glob
setopt auto_param_keys
setopt auto_list
setopt auto_menu
setopt list_types
setopt cdable_vars

## history settings
HISTFILE=$HOME/.zsh-history
HISTSIZE=100000
SAVEHIST=100000
setopt hist_ignore_all_dups
setopt hist_reduce_blanks
setopt share_history
setopt hist_no_store
setopt pushd_ignore_dups
function history-all { history -E 1 }
zstyle ':completion:*:default' menu select=1

## set TERM to use color terminal
eval `tset -sQI xterm-256color`

## prompt
setopt prompt_subst
unsetopt promptcr
# for emacs (no escape usging)
if [[ $EMACS = t ]]; then
    unsetopt zle
fi
PROMPT=$'\n''%{[36m%}%n@%m%{[m%}:%{[33m%}%~%{[m%}'$'\n''%# '
# vcs info on RPROMPT
autoload -Uz vcs_info
zstyle ':vcs_info:*' formats '(%s) %b'
zstyle ':vcs_info:*' actionformats '(%s) %b!%a'
precmd () {
    psvar=()
    LANG=$LANG vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}
RPROMPT="%1(v|%F{red}%1v%f|)"

## misc
setopt correct
setopt no_hup
setopt sun_keyboard_hack
setopt always_last_prompt
setopt sh_word_split
setopt no_flow_control

## no beep
setopt no_beep
setopt nolistbeep

## no coredump
limit coredumpsize 0

## dabbrev on screen
HARDCOPYFILE=$HOME/.screen-hardcopy
touch $HARDCOPYFILE

function dabbrev-complete() {
    local reply lines=80
    screen -X eval "hardcopy -h $HARDCOPYFILE"
    reply=($(sed '/^$/d' $HARDCOPYFILE | sed '$ d' | tail -$lines))
    compadd - "${reply[@]%[*/=@|]}"
}

zle -C dabbrev-complete menu-complete dabbrev-complete
bindkey '^o' dabbrev-complete
bindkey '^o^_' reverse-menu-complete

## encode/decode base64
function encode_base64() { perl -MMIME::Base64 -e "print encode_base64('$1')" }
function decode_base64() { perl -MMIME::Base64 -e "print decode_base64('$1')" }

## pmtools
function pmvers() { perl -m$1 -e 'print "$'$1'::VERSION\n"' }

## perl module development
function pm {
    if [ -f "Makefile.PL" ]; then
        perl Makefile.PL $@ && make
    elif [ -f "Build.PL" ]; then
        perl Build.PL $@ && ./Build
    fi
}
function pt {
    if [ -f "Makefile" ]; then
        make test $@
    elif [ -f "Build" ]; then
        ./Build test $@
    fi
}

## at sixapart
[ -f .zshrc.6a ] && source .zshrc.6a
