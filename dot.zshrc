## -*- mode: shell-script; -*-

export LANGUAGE=en_US:en
export LC_ALL=C
export LANG=en_US.UTF-8

export SHELL=`which zsh`

[ -d /usr/local/mysql ] && PATH=/usr/local/mysql/bin:$PATH
[ -d $HOME/bin ] && PATH=$HOME/bin:$PATH
## remove duplicates in PATH with keeping the order
export PATH="$(echo $PATH | awk -v RS=: -v ORS=: '!($0 in a) {a[$0]; print}')"

[[ -x `where lv` ]] && export PAGER=lv
export GIT_PAGER=cat

export EDITOR=$HOME/bin/emacsclient
export ALTERNATE_EDITOR=vi

export GREP_COLOR='07;33'
export GISTY_DIR=$HOME/dev/gists

## for mac
if [ `uname` = "Darwin" ]; then
    # set flags to compile XS module
    export ARCHFLAGS='-arch i386 -arch x86_64'
    # mysql client
    export DYLD_LIBRARY_PATH=/usr/local/mysql/lib
fi

if [[ -x `where dircolors` ]] && [ -e $HOME/.dircolors ]; then
    eval `dircolors $HOME/.dircolors`
fi

## trying to use perlbrew or local::lib
if [ -f ~/perl5/perlbrew/etc/bashrc ]; then
    source ~/perl5/perlbrew/etc/bashrc
elif [ -z "$PERL5LIB" ]; then
    eval `perl -Iperl5/lib/perl5 -Mlocal::lib 2>/dev/null`
fi

## changing title of screen's window by preexec()
if [ -n "$WINDOW" ]; then
    preexec() {
        emulate -L zsh
        local -a cmd; cmd=(${(z)2})
        case $cmd[1] in
            fg)
                if (( $#cmd == 1 )); then
                    cmd=(builtin jobs -l %+)
                else
                    cmd=(builtin jobs -l $cmd[2])
                fi
                ;;
            %*)
                cmd=(builtin jobs -l $cmd[1])
                ;;
            ls)
                return
                ;;
            cd|ssh)
                if (( $#cmd == 2)); then
                    cmd[1]=$cmd[2]
                fi
                ;&
            tail)
                if [[ $cmd[2] = '-f' ]]; then
                    cmd[1]=$cmd[3]
                fi
                ;&
            *)
                echo -n "k$cmd[1]:t\\"
                return
                ;;
        esac

        local -A jt; jt=(${(kv)jobtexts})

        $cmd >>(read num rest
            cmd=(${(z)${(e):-\$jt$num}})
            echo -n "k$cmd[1]:t\\") 2>/dev/null
    }
fi

## change the terminal window title
#precmd() { echo -n "\e]0;$USER@$HOST\a" }

## completion
autoload -U compinit
compinit
# case insensitive
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

## bash-completion
BASH_COMP_DIR=/usr/local/etc/bash_completion.d
if [ -d $BASH_COMP_DIR ]; then
    autoload bashcompinit
    bashcompinit
    source $BASH_COMP_DIR/git-completion.bash
fi

## alias
setopt complete_aliases
alias ec="$HOME/bin/emacsclient -n"
if [ `uname` = "Linux" ]; then
    alias ls="ls -F --color=auto"
else
    alias ls="ls -FG"
fi
alias l="ls -lh"
alias ll="ls -Alh"
alias sc="screen -xRU"
alias gv="grep -v .svn"
alias lv="lv -c -Ou8"
alias less="less -gj10"
alias vs="svn st -u"
alias vd="svn di"
alias vl="svn log --stop-on-copy"
alias us="TZ=US/Pacific date"
alias lc="tr '[:upper:]' '[:lower:]'"
alias uc="tr '[:lower:]' '[:upper:]'"
alias htdate="date '+%a, %d %b %Y %X %Z'"
alias prove="prove -lv"
alias ack="ack --color"
alias grep="grep --binary-files=without-match --color=always"
alias t="$HOME/bin/todo.sh -d $HOME/.todo"
# alias for git and uutocomplete for 'g' as well
alias g=git
complete -o default -o nospace -F _git g

## global alias
alias -g M="| more"
alias -g L="| lv"
alias -g H="| head"
alias -g T="| tail"
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
[ -d $HOME/.zsh.d ] || mkdir $HOME/.zsh.d
HISTFILE=$HOME/.zsh.d/history
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
#eval `tset -sQI xterm-256color`

## prompt
setopt prompt_subst
unsetopt promptcr
# for emacs (no escape usging)
if [ "$EMACS" = t ]; then
    unsetopt zle
fi
PROMPT=$'\n''%{[36m%}%n@%m%{[m%}:%{[33m%}%~%{[m%}'$'\n''%# '
# vcs info on RPROMPT
autoload -Uz vcs_info
zstyle ':vcs_info:*' formats '(%s) %b'
zstyle ':vcs_info:*' actionformats '(%s) %b!%a'
precmd () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
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
HARDCOPYFILE=$HOME/.zsh.d/screen-hardcopy
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

## pmtools
function pmver() { perl -m$1 -e 'print "$'$1'::VERSION\n"' }

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
