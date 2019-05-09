## -*- mode: shell-script; -*-

autoload -Uz add-zsh-hook

export SHELL=`which zsh`

if which dircolors > /dev/null; then
    eval "$(dircolors ~/.dircolors)"
fi

########################################
## Development
########################################

function plenv() {
    eval "$(command plenv init -)"
    plenv "$@"
}

function rbenv() {
    eval "$(command rbenv init -)"
    rbenv "$@"
}

typeset -U PATH

########################################
## Tmux
########################################

if [[ -n "$TMUX" ]]; then
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

        local -a jt; jt=(${(kv)jobtexts})

        $cmd >>(read num rest
            cmd=(${(z)${(e):-\$jt$num}})
            echo -n "k$cmd[1]:t\\") 2>/dev/null
    }
    add-zsh-hook preexec _update_window_title

    ## dabbrev using current pane contents
    function _dabbrev_from_pane() {
        local -a sources
        sources=($(tmux capture-pane\; show-buffer \; delete-buffer | sed '/^$/d' | sed '$ d'))
        compadd - "${sources[@]%[*/=@|]}"
    }
    zle -C dabbrev-from-pane menu-complete _dabbrev_from_pane
    bindkey '^o' dabbrev-from-pane
fi

########################################
## Aliases
########################################

setopt no_complete_aliases

alias emacsclient="TERM=xterm-256color-italic emacsclient"
alias emacsd="emacs -nw --daemon && emacsclient -t"
alias ls="ls -F --color=auto --group-directories-first"
alias l="ls -lh"
alias ll="ls -Alh"
alias lc="tr '[:upper:]' '[:lower:]'"
alias uc="tr '[:lower:]' '[:upper:]'"
alias prove="prove -lv --timer"
alias grep="grep --binary-files=without-match --color=auto"
alias cpanl="cpanm --mirror ~/minicpan --mirror-only"
alias b="bundle exec"
alias py="python3"
alias f="gfind"

if which colordiff > /dev/null; then
    alias diff="colordiff -u"
else
    alias diff='diff -u'
fi

## git
if which hub > /dev/null; then
    alias git="hub"
fi
alias g="git"

## docker
alias d="docker"
alias c="docker-compose"
alias m="docker-machine"

## vagrant
alias v="vagrant"
alias vup="vagrant up && vagrant ssh"

## aws
alias production="aws --profile=production"
alias staging="aws --profile=staging"

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

## zmv
autoload -Uz zmv
alias zmv="noglob zmv -W"

########################################
## Completion
########################################

setopt auto_param_keys
setopt auto_remove_slash
setopt auto_list
setopt auto_menu
setopt list_types

## additional completions by https://github.com/zsh-users/zsh-completions
if [[ -d /usr/local/share/zsh-completions ]]; then
    fpath=(/usr/local/share/zsh-completions $fpath)
fi

## init completion with reducing checking zcompdump file
autoload -Uz compinit
if [[ -n ~/.zcompdump(#qN.mh+24) ]]; then
    compinit
else
    compinit -C
fi

## case insensitive at completion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
## colorized completion
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
## ignore current directory from ../
zstyle ':completion:*' ignore-parents parent pwd ..

## awscli installed by homebrew
test -f /usr/local/share/zsh/site-functions/_aws && source $_
## travis completion by travis gem
test -f ~/.travis/travis.sh && source $_

########################################
## History
########################################

readonly HISTFILE=~/.zsh/history
readonly HISTSIZE=100000
readonly SAVEHIST=100000
setopt hist_ignore_all_dups
setopt hist_reduce_blanks
setopt hist_ignore_space
setopt share_history
setopt hist_no_store
setopt pushd_ignore_dups
zstyle ':completion:*:default' menu select=1

########################################
## Prompt
########################################

setopt prompt_subst
unsetopt promptcr

# for emacs (no escape usging)
if [[ "$EMACS" == t ]]; then
    unsetopt zle
fi

## vcs info
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' unstagedstr '%F{160}%B✖︎ %b%f'
zstyle ':vcs_info:git:*' stagedstr '%F{155}%B✔︎︎ %b%f'
zstyle ':vcs_info:*' formats '%F{117}(%s:%b)%f %c%u%m'
zstyle ':vcs_info:*' actionformats '%F{160}(%s:%b!%a)%f %c%u%m'
zstyle ':vcs_info:git*+set-message:*' hooks git-st git-stash

## Show remote ref name and number of commits ahead-of or behind
function +vi-git-st () {
    ## get remote's "repos/branch"
    local remote=$(command git rev-parse --abbrev-ref --symbolic-full-name @{u} 2>/dev/null)

    if [[ -n "$remote" ]]; then
        local -a gitstatus
        local ahead=$(command git rev-list ${hook_com[branch]}@{u}..HEAD --count 2>/dev/null)
        (( $ahead )) && gitstatus+=( "+$ahead" )

        local behind=$(command git rev-list HEAD..${hook_com[branch]}@{u} --count 2>/dev/null)
        (( $behind )) && gitstatus+=( "-$behind" )

        if [[ -n "$gitstatus" ]]; then
            hook_com[branch]="${hook_com[branch]} %U${(j:/:)gitstatus}%u"
        fi
    fi
}
## show count of stashed
function +vi-git-stash() {
    local -a stashes
    if [[ -s ${hook_com[base]}/.git/refs/stash ]] ; then
        stashes=$(command git stash list 2>/dev/null | wc -l | sed -e 's/ //g')
        (( $stashes )) && hook_com[misc]+="%F{252}☁ ${stashes}%f";
    fi
}
function _precmd_vcs_info () {
    LANG=en_US.UTF-8 vcs_info
}
add-zsh-hook precmd _precmd_vcs_info

function _prompt_cwd() {
    if [[ $UID -eq 0 ]]; then
        echo -n '%F{255}%K{160}%~%k%f'
    else
        echo -n '%F{214}%~%f'
    fi
}
PROMPT=$'\n''$(_prompt_cwd) ${vcs_info_msg_0_}'$'\n''%(?,➜ ,%F{226}⚠ %f)'

## command line coloring
zle_highlight=(isearch:underline,fg=red region:fg=black,bg=yellow special:standout,fg=blue suffix:bold)

########################################
## cdr
########################################

autoload -Uz chpwd_recent_dirs cdr

function _post_chpwd {
    chpwd_recent_dirs
    ls
}
add-zsh-hook chpwd _post_chpwd

zstyle ':chpwd:*' recent-dirs-max 500
zstyle ':chpwd:*' recent-dirs-file "$HOME/.zsh/chpwd-recent-dirs"
zstyle ':chpwd:*' recent-dirs-pushd true

########################################
## zaw
########################################
source ~/.zaw/zaw.zsh

zstyle ':filter-select:highlight' selected fg=255,bg=24
zstyle ':filter-select:highlight' title fg=226
zstyle ':filter-select:highlight' matched fg=117
zstyle ':filter-select' case-insensitive yes
zstyle ':filter-select' max-lines 10
zstyle ':filter-select' extended-search yes

bindkey '^x^r' zaw-history
bindkey '^x^f' zaw-git-files-legacy

## checkout recent used branch
function zaw-src-git-recent-branches () {
    command git rev-parse --git-dir >/dev/null 2>&1
    if [[ $? == 0 ]]; then
        candidates=( $(command git for-each-ref --format='%(refname:short)' --sort=-committerdate refs/heads) )
    fi
    actions=(zaw-src-git-recent-branches-checkout)
    act_descriptions=("check out")
}
function zaw-src-git-recent-branches-checkout () {
    BUFFER="git checkout $1"
    zle accept-line
}
zaw-register-src -n git-recent-branches zaw-src-git-recent-branches
bindkey '^x^b' zaw-git-recent-branches

## ghq list directories source
function zaw-src-ghq-cdr() {
    candidates=( $(command ghq list -p) $(cdr -l | awk '{print $2}') )
    actions=("zaw-src-cdr-cd")
    act_descriptions=("cd")
    return 0
}
zaw-register-src -n ghq-cdr zaw-src-ghq-cdr
bindkey '^xb' zaw-ghq-cdr

## completion strings like a filename displayed in current tmux pane
function zaw-src-tmux-pane-strings() {
    candidates=($(tmux capture-pane\; show-buffer \; delete-buffer | perl -pne 's/ +/\n/g; print' | sort -u | \grep '[\.\/]'))
    actions=("zaw-callback-append-to-buffer")
    act_descriptions=("append to edit buffer")
    return 0
}
zaw-register-src -n tmux-pane-strings zaw-src-tmux-pane-strings
bindkey '^x^o' zaw-tmux-pane-strings

## perldoc finding from local/lib/perl5
function zaw-callback-perldoc-emacs() {
    local orig_buffer="${BUFFER}"
    BUFFER="emacsclient -t $(perldoc -lm "$1")"
    zle accept-line
}
function zaw-src-perldoc-local() {
    # global modules
    zaw-src-perldoc

    # local modules installed by carton into local/lib/perl5
    local -a carton
    if [[ -d local/lib/perl5 ]]; then
        carton=($(command find local/lib/perl5 -type f -name '*.pm' -or -name '*.pod'))
    fi

    candidates=($carton $candidates)
    actions=("zaw-callback-perldoc-view" "zaw-callback-perldoc-emacs")
    act_descriptions=("view perldoc" "open with emacs")
}
zaw-register-src -n perldoc-local zaw-src-perldoc-local
bindkey '^x^p' zaw-perldoc-local

########################################
## My Functions
########################################

fpath=(~/.zsh/functions $fpath)

autoload -Uz pmver

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

########################################
## zsh-syntax-highlighting
########################################
ZSH_SYNTAX_HIGHLIGHTING_SRC=/usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
eval "source $ZSH_SYNTAX_HIGHLIGHTING_SRC"

## tweak styles
ZSH_HIGHLIGHT_STYLES[unknown-token]=fg:red,underline
ZSH_HIGHLIGHT_STYLES[path]=none
ZSH_HIGHLIGHT_STYLES[path_approx]=none

## highlighters
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)

########################################
## compile for speed-up
########################################
compile_targets=(~/.zshrc $ZSH_SYNTAX_HIGHLIGHTING_SRC)
for f in $compile_targets; do
    if [ ! -f "${f}.zwc" -o $f -nt "${f}.zwc" ]; then
        zcompile $f
    fi
done

########################################
## Profiling by zprof
########################################
if which zprof > /dev/null; then
    zprof | less
fi
