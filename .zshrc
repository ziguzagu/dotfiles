## -*- mode: shell-script; -*-

# prepare zprof for profzsh command
if [[ $ZPROF == 'true' ]]; then
    zmodload zsh/zprof
fi

# emacs mode
bindkey -e

# Changing Directories
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
# Completion
setopt auto_name_dirs
setopt no_list_beep
# Expansion and Globbing
setopt extended_glob
# History
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt hist_no_store
setopt hist_reduce_blanks
setopt share_history
# Input/Output
setopt correct
setopt no_flow_control
# Job Control
setopt no_hup
# Prompting
setopt prompt_subst
setopt no_promptcr
# Shell Emulation
setopt sh_word_split
# Zle
setopt no_beep

# no coredump needed
limit coredumpsize 0

autoload -Uz add-zsh-hook

# PATH
eval $(brew shellenv)
if [[ -x "$(which plenv)" ]]; then
    eval "$(plenv init -)"
fi
if [[ -x "$(which rbenv)" ]]; then
    eval "$(rbenv init -)"
fi
typeset -U PATH

# Environment Variables
if [[ -x "$(which gdircolors)" ]]; then
    eval "$(gdircolors ~/.config/dircolors)"
fi
export GO111MODULE=on
export GOBIN=$HOME/bin
export GOMODCACHE=$HOME/.cache/go_mod
export GPG_TTY=$(tty)
export GREP_COLORS="ms=04;31:mc=01;33:sl=:cx=:fn=33:ln=33:bn=33:se=01;30"
export LESSCHARSET=utf-8
export LESS_TERMCAP_mb=$'\e[1;31m'     # begin blinking
export LESS_TERMCAP_md=$'\e[1;33m'     # begin bold
export LESS_TERMCAP_me=$'\e[0m'        # end mode
export LESS_TERMCAP_se=$'\e[0m'        # end standout-mode
export LESS_TERMCAP_so=$'\e[0;30;47m'  # begin standout-mode
export LESS_TERMCAP_ue=$'\e[0m'        # end underline
export LESS_TERMCAP_us=$'\e[4;34m'     # begin underline
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export SHELL="$(which zsh)"

########################################
## Tmux
########################################

_rename_tmux_window() {
    [[ -z "$TMUX" ]] && return

    local -a cmd; cmd=(${(z)2})
    local title="$cmd[1]"
    local project=${$(command git rev-parse --show-toplevel 2>/dev/null)##*/}
    if [[ -z "$project" ]]; then
        tmux rename-window "$title"
    elif [[ -z "$title" ]]; then
        tmux rename-window "‹$project›"
    else
        tmux rename-window "‹$project›$title"
    fi
}

add-zsh-hook preexec _rename_tmux_window

########################################
## Aliases
########################################

## overwrite commands with better defaults
alias bat="bat --theme=TwoDark --italic-text=always"
alias diff="colordiff -u"
alias emacs="emacs --daemon && emacsclient -t"
alias fzf="fzf --color=dark,gutter:0 --height=14 --reverse --bind=ctrl-g:print-query"
alias grep="grep -I --color=auto"
alias less="less -giMqR -j10"
alias ls="gls -AXF --color=auto --group-directories-first"
alias pgrep="pgrep -afil"
alias prove="prove -lv --timer"
alias rg="rg --heading --colors=match:fg:214 --colors=match:style:nobold --colors=match:style:underline --colors=line:fg:117 --colors=path:fg:251 --colors=path:bg:238"
alias ssh="TERM=xterm-256color ssh"

## shortcuts
alias b="bat"
alias c="docker-compose"
alias ci="circleci"
alias d="docker"
alias g="git"
alias l="ls -lh"
alias p="less"
alias s="rg"
alias t="tig"
alias tf="terraform"
alias z="fzf"

## global aliases
alias -g P="| less"

########################################
## Completion
########################################

## additional completions by https://github.com/zsh-users/zsh-completions
if [[ -d $HOMEBREW_PREFIX/share/zsh-completions ]]; then
    fpath=($HOMEBREW_PREFIX/share/zsh-completions $fpath)
fi

## init completion with reducing checking zcompdump file
autoload -Uz compinit
if [[ -n ~/.zcompdump(\#qN.mh+24) ]]; then
    compinit
else
    compinit -C
fi

zstyle ':completion:*' cache-patch ~/.cache/zsh/zcompcache
## case insensitive at completion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
## colorized completion
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
## ignore current directory from ../
zstyle ':completion:*' ignore-parents parent pwd ..

## travis completion by travis gem
test -f ~/.travis/travis.sh && source $_

########################################
## History
########################################

HISTFILE=$XDG_DATA_HOME/zsh/history
HISTSIZE=100000
SAVEHIST=100000

zstyle ':completion:*:default' menu select=1

########################################
## Prompt
########################################

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' unstagedstr '%F{160}%B✖︎ %b%f'
zstyle ':vcs_info:git:*' stagedstr '%F{155}%B✔︎︎ %b%f'
zstyle ':vcs_info:*' formats '%F{117}(%s:%b)%f %c%u%m'
zstyle ':vcs_info:*' actionformats '%F{160}(%s:%b!%a)%f %c%u%m'
zstyle ':vcs_info:git*+set-message:*' hooks git-st git-stash

## Show remote ref name and number of commits ahead-of or behind
+vi-git-st() {
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
+vi-git-stash() {
    local -a stashes
    if [[ -s ${hook_com[base]}/.git/refs/stash ]] ; then
        stashes=$(command git stash list 2>/dev/null | wc -l | sed -e 's/ //g')
        (( $stashes )) && hook_com[misc]+="%F{252}☁ ${stashes}%f";
    fi
}

add-zsh-hook precmd vcs_info

_prompt_cwd() {
    if [[ $UID -eq 0 ]]; then
        echo -n '%F{254}%K{160}%~%k%f'
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

_post_chpwd() {
    chpwd_recent_dirs
    ls
}
add-zsh-hook chpwd _post_chpwd

zstyle ':chpwd:*' recent-dirs-default true
zstyle ':chpwd:*' recent-dirs-max 500
zstyle ':chpwd:*' recent-dirs-file "$XDG_DATA_HOME/zsh/chpwd-recent-dirs"
zstyle ':chpwd:*' recent-dirs-pushd true

# remove non-existing directories from recent-dirs-file
cdr-gc() {
    local abs
    cdr -l | perl -pe 's/^\d+\s+//' | while read dir; do
        abs=${dir:s/~/$HOME/}
        if [[ ! -d $abs ]]; then
            cdr -P "$dir"
        fi
    done
}

########################################
## fzf
########################################

# search from history
fzf-history() {
    local cmd="$(fc -lnr 1 | fzf +m -e --tiebreak=index --bind=ctrl-r:down --bind=ctrl-s:up --query=$LBUFFER)"
    if [[ -z "$cmd" ]]; then
        zle redisplay
        return 0
    fi
    LBUFFER="$cmd"
    zle reset-prompt
}
zle -N fzf-history
bindkey '^r' fzf-history

# return 0 if the current directory is git repo
_is_in_git_repo() {
    git rev-parse --git-dir >& /dev/null
}

# select files and directories in current directory
fzf-ls() {
    set -o pipefail
    local -a list=($(gls -AlhFX --group-directories-first \
                         | fzf --header-lines=1 +s -m --nth=-1 \
                               --bind='ctrl-v:toggle-preview' \
                               --preview-window=hidden \
                               --preview='test -d {-1} && gls -lhF --group-directories-first $_ || bat --color=always --style=numbers $_' \
                         | awk '{print $NF}'))
    LBUFFER+=$list
    zle reset-prompt
    return $ret
}
zle -N fzf-ls
bindkey '^x^f' fzf-ls

# search from git ls-files
fzf-git-ls-files() {
    _is_in_git_repo || return
    local -a files=($(git ls-files \
                          | fzf -m -e \
                                --bind='ctrl-v:toggle-preview' \
                                --preview-window=hidden \
                                --preview='test -d {-1} && gls -lhF --group-directories-first $_ || bat --color=always --style=numbers $_'))
    local ret=$?
    LBUFFER+=$files
    zle reset-prompt
    return $ret
}
zle -N fzf-git-ls-files
bindkey '^xf' fzf-git-ls-files

# select untracked files or changed files
fzf-git-untracked-or-changed-files() {
    _is_in_git_repo || return
    set -o pipefail
    local -a files=($(git status -s | fzf -m -e --height=40% --preview='git preview {2}' | perl -pe 's/^\s*..\s+//'))
    local ret=$?
    LBUFFER+=$files
    zle reset-prompt
    return $ret
}
zle -N fzf-git-untracked-or-changed-files
bindkey '^xv' fzf-git-untracked-or-changed-files

# git switch to selected branches in recent used
fzf-git-switch-recent-branch() {
    _is_in_git_repo || return
    local branch="$(git for-each-ref --format='%(refname:short)' --sort=-committerdate refs/heads | fzf +m -e --tiebreak=index)"
    if [[ -z "$branch" ]]; then
        zle redisplay
        return 0
    fi
    BUFFER="git switch $branch"
    zle reset-prompt
    zle accept-line
}
zle -N fzf-git-switch-recent-branch
bindkey '^xb' fzf-git-switch-recent-branch

# jump to directory selected from ghq / cdr
fzf-jump-directory() {
    local -a dirs=(
        $(cdr -l | perl -pe 's/^\d+\s+//')
        $(ghq list -p | perl -pe 's/^\Q$ENV{HOME}\E/~/')
    )
    local dir="$(print -l ${(u)dirs} | fzf +m -e --tiebreak=index)"
    if [[ -z "$dir" ]]; then
        zle redisplay
        return 0
    fi
    BUFFER="cd $dir"
    zle reset-prompt
    zle accept-line
}
zle -N fzf-jump-directory
bindkey '^j' fzf-jump-directory

# find strings from current tmux pane
fzf-find-strings-from-tmux-pane() {
    local -a strings=($(tmux capture-pane -p -S -40 | stringus | fzf --reverse +m -e --tiebreak=index))
    LBUFFER+=$strings
    zle reset-prompt
}
zle -N fzf-find-strings-from-tmux-pane
bindkey '^o' fzf-find-strings-from-tmux-pane

# find perl modules of core and bundled by carton
fzf-find-perl-module() {
    local -a candidates

    # Find core perl modules using _perl_modules zsh completion
    #
    # XXX: override _wanted to capture module list _perl_modules generates
    local code_wanted="${functions[_wanted]}"
    _wanted() {
        candidates=("${(P@)${@[7]}}")
    }
    # required by _perl_modules
    local -a words=(perldoc)
    _perl_modules

    # restore original function
    eval "function _wanted() { $code_wanted }"

    # find local modules installed by carton into local/lib/perl5
    if [[ -d local/lib/perl5 ]]; then
        candidates+=($(find local/lib/perl5 -type f -name '*.pm' -or -name '*.pod'))
    fi

    # doing fzf
    local pm="$(print -l $candidates | fzf +m)"
    LBUFFER+="$pm"
    zle reset-prompt
}
zle -N fzf-find-perl-module
bindkey '^x^p' fzf-find-perl-module

########################################
## Misc
########################################

## zsh-syntax-highlighting
source $HOMEBREW_PREFIX/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
ZSH_HIGHLIGHT_STYLES[unknown-token]=fg:red,underline
ZSH_HIGHLIGHT_STYLES[path]=none
ZSH_HIGHLIGHT_STYLES[path_approx]=none
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)

## compile
if [ ! -f "~/.zshrc.zwc" -o "~/.zshrc" -nt "~/.zshrc.zwc" ]; then
    zcompile ~/.zshrc
fi

eval "$(direnv hook zsh)"

########################################
## Command
########################################

# get rid of ANSI escape sequences to pipe STDOUT to pbcopy
decolor() {
    perl -pe 's/\e\[\d+m//g'
}

lc() {
    tr '[:upper:]' '[:lower:]'
}

pmver() {
    readonly pkg="$1"
    perl -m${pkg} -E "say \$${pkg}::VERSION"
}

uc() {
    tr '[:lower:]' '[:upper:]'
}

########################################
## Profiling by zprof
########################################
profzsh() {
    ZPROF=true $SHELL -i -c exit
}

if [[ $ZPROF == "true" ]]; then
    zprof
fi
