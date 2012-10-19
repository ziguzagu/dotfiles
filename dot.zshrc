## -*- mode: shell-script; -*-

export LANGUAGE=en_US:en
export LANG=en_US.UTF-8

export SHELL=`which zsh`

## setup PATH without duplicates path
PATH=$HOME/bin:$HOME/.rvm/bin:/usr/local/bin:$PATH
export PATH="$(echo -n $PATH | awk -v RS=: -v ORS=: '!($0 in a) {a[$0]; print}')"

## page and editor
if which lv > /dev/null; then
    export PAGER=lv
fi
export GIT_PAGER=cat
export EDITOR=$HOME/bin/ec-wait
export ALTERNATE_EDITOR=vi

export GREP_COLOR='07;33'
export GISTY_DIR=$HOME/dev/gists

## set flags to compile XS module on MacOS X
if [ `uname` = "Darwin" ]; then
    export ARCHFLAGS='-arch i386 -arch x86_64'
fi

if [[ -x `where dircolors` ]] && [ -e $HOME/.dircolors ]; then
    eval `dircolors $HOME/.dircolors`
fi

## trying to use perlbrew or local::lib
if [ -f ~/perl5/perlbrew/etc/bashrc ]; then
    source ~/perl5/perlbrew/etc/bashrc
    export MANPATH="$PERLBREW_ROOT/perls/$PERLBREW_PERL/man:$MANPATH"
elif [ -z "$PERL5LIB" ]; then
    eval `perl -Iperl5/lib/perl5 -Mlocal::lib 2>/dev/null`
fi

## rbenv
if which rbenv > /dev/null; then
    eval "$(rbenv init -)"
fi

## changing title of tmux/screen window by preexec()
if [ -n "$WINDOW" ] || [ -n "$TMUX" ]; then
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

## completion
autoload -U compinit
compinit
# case insensitive
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

## alias
setopt complete_aliases
#alias ec="$HOME/bin/emacsclient -n"
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
alias prove="prove -lv --timer"
alias ack="ack --color"
alias grep="grep --binary-files=without-match --color=always"

## git completion
alias g="git"
autoload bashcompinit
bashcompinit
source ~/.zsh.d/git-completion.bash

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
if [ -n "$TMUX" ]; then
    export TERM=screen-256color
fi

## prompt
autoload -U colors && colors
setopt prompt_subst
unsetopt promptcr
# for emacs (no escape usging)
if [ "$EMACS" = t ]; then
    unsetopt zle
fi
# vcs info
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' unstagedstr '%F{yellow}￭￭%f'
zstyle ':vcs_info:git:*' stagedstr '%F{red}￭￭%f'
zstyle ':vcs_info:*' formats ' %F{green}(%s:%b)%f %c%u'
zstyle ':vcs_info:*' actionformats ' %F{green}(%s:%b!%a)%f %c%u'
## find commits not pushed yet
_git_has_not_pushed_commit () {
    if [ "$(git remote 2>/dev/null)" ]; then
        local head="$(git rev-parse HEAD 2>/dev/null)"
        local remote
        for remote in $(git rev-parse --remotes 2>/dev/null); do
            if [ "$head" != "$remote" ]; then
                echo 1
                return
            fi
        done
    fi
}
## setup builtin vcs_info and my vcs_info
precmd () {
    LANG=en_US.UTF-8 vcs_info

    my_vcs_info=""
    if [ -n "$(_git_has_not_pushed_commit)" ]; then
        my_vcs_info="${my_vcs_info}%F{blue}￭￭%f"
    fi
}

# set prompt var for root/user
if [ $UID -eq 0 ]; then
    PROMPT=$'\n''%{$fg[red]%}%n@%m%{$reset_color%}:%{$fg[yellow]%}%~%{$reset_color%}${vcs_info_msg_0_}${my_vcs_info}'$'\n''➜ '
else
    PROMPT=$'\n''%{$fg[cyan]%}%n@%m%{$reset_color%}:%{$fg[yellow]%}%~%{$reset_color%}${vcs_info_msg_0_}${my_vcs_info}'$'\n''➜ '
fi

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
if [ -f ~/.zshrc.6a ]; then
    source ~/.zshrc.6a
fi
