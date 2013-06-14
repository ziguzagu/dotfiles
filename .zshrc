## -*- mode: shell-script; -*-

export SHELL=`which zsh`

## setup perl env, trying to use plenv then local::lib
if which plenv > /dev/null; then
    eval "$(plenv init -)"
elif [ -z "$PERL5LIB" ]; then
    eval `perl -Iperl5/lib/perl5 -Mlocal::lib 2>/dev/null`
fi
## ruby and python
if which rbenv > /dev/null; then
    eval "$(rbenv init -)"
fi
if which pyenv > /dev/null; then
    eval "$(pyenv init -)"
fi

if which dircolors > /dev/null; then
    eval "$(dircolors ~/.dircolors)"
fi

## hive installed by homebrew on mac
if which hive > /dev/null; then
    export HIVE_HOME=/usr/local/Cellar/hive/0.9.0/libexec
fi

## changing title of tmux window by preexec()
if [ -n "$TMUX" ]; then
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

## completion files and directories (without secure check -u)
autoload -U compinit
compinit -u
# case insensitive
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

## alias
setopt complete_aliases
alias emacs="emacs --daemon && emacsclient -t"
alias ls="ls -F --color=auto"
alias l="ls -lh"
alias ll="ls -Alh"
alias sc="screen -xRU"
alias gv="grep -v .svn"
alias vs="svn st -u"
alias vd="svn di"
alias vl="svn log --stop-on-copy"
alias us="TZ=US/Pacific date"
alias lc="tr '[:upper:]' '[:lower:]'"
alias uc="tr '[:lower:]' '[:upper:]'"
alias htdate="date '+%a, %d %b %Y %X %Z'"
alias prove="prove -lv --timer"
alias ack="ack --color"
alias grep="grep --binary-files=without-match --color=auto"

## git completion work with alias for git
alias g="git"
source ~/.zsh.d/git-completion.bash
complete -o default -o nospace -F _git g

## global alias
alias -g M="| more"
alias -g L="| less"
alias -g H="| head"
alias -g T="| tail"
alias -g G="| grep"
alias -g GV="| grep -v"
alias -g N="> /dev/null 2>&1"

## sets keybind like emacs
bindkey -e

## directory handlings
setopt auto_cd
setopt auto_pushd
setopt auto_name_dirs
setopt extended_glob

## complete setting
setopt auto_param_keys
setopt auto_remove_slash
setopt auto_list
setopt auto_menu
setopt list_types

## history settings
[ -d $HOME/.zsh.d ] || mkdir $HOME/.zsh.d
HISTFILE=$HOME/.zsh.d/history
HISTSIZE=100000
SAVEHIST=100000
setopt hist_ignore_all_dups
setopt hist_reduce_blanks
setopt hist_ignore_space
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
autoload -Uz add-zsh-hook
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' unstagedstr '%F{white}￭￭%f'
zstyle ':vcs_info:git:*' stagedstr '%F{red}￭￭%f'
zstyle ':vcs_info:*' formats '%F{yellow}(%s:%b)%f %c%u'
zstyle ':vcs_info:*' actionformats '%F{red}(%s:%b!%a)%f %c%u'
zstyle ':vcs_info:git*+set-message:*' hooks git-st
## Show remote ref name and number of commits ahead-of or behind
function +vi-git-st () {
    ## get remote's "repos/branch"
    local remote=${$(git rev-parse --verify ${hook_com[branch]}@{upstream} --symbolic-full-name 2>/dev/null)/refs\/remotes\/}

    if [[ -n "$remote" ]]; then
        local -a gitstatus
        local ahead=${$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | wc -l)/\s/}
        (( $ahead )) && gitstatus+=( "+$ahead" )

        local behind=${$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | wc -l)/\s/}
        (( $behind )) && gitstatus+=( "-$behind" )

        if [[ -n "$gitstatus" ]]; then
            hook_com[branch]="${hook_com[branch]} %U${(j:/:)gitstatus}%u"
        fi
    fi
}
function _precmd_vcs_info () {
    LANG=en_US.UTF-8 vcs_info
}
add-zsh-hook precmd _precmd_vcs_info

## use different style prompt betwen root and user
if [ $UID -eq 0 ]; then
    PROMPT=$'\n''%F{red}%S%n@%m:%~%s'$'\n''➜ %f'
else
    PROMPT=$'\n''%B%F{red}%n@%m:%f%b%F{cyan}%~%f ${vcs_info_msg_0_}'$'\n''➜ '
fi

## command line coloring
zle_highlight=(isearch:underline,fg=red region:fg=black,bg=yellow special:standout,fg=blue suffix:bold)

## misc
setopt correct
setopt no_hup
setopt always_last_prompt
setopt sh_word_split
setopt no_flow_control

## no beep
setopt no_beep
setopt no_list_beep

## no coredump
limit coredumpsize 0

## z
if [ -f /usr/local/etc/profile.d/z.sh ]; then
    source /usr/local/etc/profile.d/z.sh
fi

## dabbrev using current pane contents
function _dabbrev_from_pane() {
    local sources
    sources=($(tmux capture-pane\; show-buffer \; delete-buffer | sed '/^$/d' | sed '$ d'))
    compadd - "${sources[@]%[*/=@|]}"
}
zle -C dabbrev-from-pane menu-complete _dabbrev_from_pane
bindkey '^o' dabbrev-from-pane

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

