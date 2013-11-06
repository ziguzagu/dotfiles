## -*- mode: shell-script; -*-

autoload -Uz add-zsh-hook

export SHELL=`which zsh`

if which dircolors > /dev/null; then
    eval "$(dircolors ~/.dircolors)"
fi

########################################
## Development
########################################

## trying to use plenv then local::lib
if which plenv > /dev/null; then
    eval "$(plenv init -)"
elif [ -z "$PERL5LIB" ]; then
    eval `perl -Iperl5/lib/perl5 -Mlocal::lib 2>/dev/null`
fi

if which rbenv > /dev/null; then
    eval "$(rbenv init -)"
fi
if which pyenv > /dev/null; then
    eval "$(pyenv init -)"
fi

## hive installed by homebrew on mac
if which hive > /dev/null; then
    export HIVE_HOME=/usr/local/Cellar/hive/0.9.0/libexec
fi

########################################
## Tmux
########################################

if [ -n "$TMUX" ]; then

    ## set TERM to use color terminal
    export TERM=screen-256color
    
    ## changing title of tmux window on executing command
    function _update_window_title() {
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
    add-zsh-hook preexec _update_window_title

    ## dabbrev using current pane contents
    function _dabbrev_from_pane() {
        local sources
        sources=($(tmux capture-pane\; show-buffer \; delete-buffer | sed '/^$/d' | sed '$ d'))
        compadd - "${sources[@]%[*/=@|]}"
    }
    zle -C dabbrev-from-pane menu-complete _dabbrev_from_pane
    bindkey '^o' dabbrev-from-pane
fi

########################################
## Aliases
########################################

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
alias prove="prove -lv --timer"
alias ack="ack --color"
alias grep="grep --binary-files=without-match --color=auto"
alias ce="carton exec"
alias minicpanm="cpanm --mirror ~/minicpan --mirror-only"

## global alias
alias -g M="| more"
alias -g L="| less"
alias -g H="| head"
alias -g G="| grep"
alias -g GV="| grep -v"

## sets keybind like emacs
bindkey -e

## directory handlings
setopt auto_cd
setopt auto_pushd
setopt auto_name_dirs
setopt extended_glob

## ls after cd
function _post_chpwd() { ls }
add-zsh-hook chpwd _post_chpwd

########################################
## Completion
########################################

setopt auto_param_keys
setopt auto_remove_slash
setopt auto_list
setopt auto_menu
setopt list_types

## additional completions by https://github.com/zsh-users/zsh-completions
if [ -d /usr/local/share/zsh-completions ]; then
    fpath=(/usr/local/share/zsh-completions $fpath)    
fi

## git completion for linux (not installed zsh/git by homebrew)
if [ "$(uname)" != "Darwin" ]; then
    zstyle ':completion:*:*:git:*' script ~/.zsh.d/completion/git-completion.bash
    fpath=(~/.zsh.d/completion $fpath)
fi

## init completion (without secure check -u)
autoload -U compinit && compinit -u

## case insensitive at completion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

## git
if which hub > /dev/null; then
    alias git="hub"
fi
alias g="git"
compdef g=git

########################################
## History
########################################

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

########################################
## Prompt
########################################

autoload -U colors && colors
setopt prompt_subst
unsetopt promptcr

# for emacs (no escape usging)
if [ "$EMACS" = t ]; then
    unsetopt zle
fi

## vcs info
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' unstagedstr '%F{white}￭￭%f'
zstyle ':vcs_info:git:*' stagedstr '%F{red}￭￭%f'
zstyle ':vcs_info:*' formats '%F{yellow}(%s:%b)%f %c%u %F{magenta}%m%f'
zstyle ':vcs_info:*' actionformats '%F{red}(%s:%b!%a)%f %c%u %F{magenta}%m%f'
zstyle ':vcs_info:git*+set-message:*' hooks git-st git-stash

## Show remote ref name and number of commits ahead-of or behind
function +vi-git-st () {
    ## get remote's "repos/branch"
    local remote=${$(\git rev-parse --verify ${hook_com[branch]}@{upstream} --symbolic-full-name 2>/dev/null)/refs\/remotes\/}

    if [[ -n "$remote" ]]; then
        local -a gitstatus
        local ahead=${$(\git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | wc -l)/\s/}
        (( $ahead )) && gitstatus+=( "+$ahead" )

        local behind=${$(\git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | wc -l)/\s/}
        (( $behind )) && gitstatus+=( "-$behind" )

        if [[ -n "$gitstatus" ]]; then
            hook_com[branch]="${hook_com[branch]} %U${(j:/:)gitstatus}%u"
        fi
    fi
}
## show count of stashed
function +vi-git-stash() {
    local -a stashes counter
    if [[ -s ${hook_com[base]}/.git/refs/stash ]] ; then
        stashes=$(git stash list 2>/dev/null | wc -l)
        (( $stashes )) && hook_com[misc]+="stashed:${stashes}";
    fi
}
function _precmd_vcs_info () {
    LANG=en_US.UTF-8 vcs_info
}
add-zsh-hook precmd _precmd_vcs_info

## build prompt string
function _render_prompt () {
    local -a host_attr path_attr cursor
    cursor='%(?,➜ ,✘ )'
    if [ $UID -eq 0 ]; then
        host_attr='%{\e[0;38;5;255;48;5;160m%}'
        path_attr=''
    else
        host_attr='%{\e[0;38;5;214m%}'
        path_attr='%{\e[0;38;5;117m%}'
    fi
    echo -n "${host_attr}%n@%m:${path_attr}%~%{\e[0m%} ${vcs_info_msg_0_}\n$cursor"
}
PROMPT=$'\n''$(_render_prompt)'

## command line coloring
zle_highlight=(isearch:underline,fg=red region:fg=black,bg=yellow special:standout,fg=blue suffix:bold)

########################################
## Misc
########################################

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

## pmtools
function pmver() { perl -m$1 -e 'print "$'$1'::VERSION\n"' }

## at sixapart
if [ -f ~/.zshrc.6a ]; then
    source ~/.zshrc.6a
fi

