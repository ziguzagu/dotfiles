## -*- mode: shell-script; -*-

autoload -Uz add-zsh-hook

export SHELL="$(which zsh)"

if [[ -x "$(which dircolors)" ]]; then
  eval "$(dircolors ~/.dircolors)"
fi

########################################
## Development
########################################

perl() {
  unset -f perl
  eval "$(command plenv init -)"
  perl "$@"
}

ruby() {
  unset -f ruby
  eval "$(command rbenv init -)"
  ruby "$@"
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
alias cpanl="cpanm --mirror ~/minicpan --mirror-only"
alias py="python3"
alias f="gfind"
alias b="bat"

## grep
alias grep="grep --binary-files=without-match --color=auto"
export GREP_COLORS="ms=04;31:mc=01;33:sl=:cx=:fn=33:ln=33:bn=33:se=01;30"

## diff
if [[ -x "$(which colordiff)" ]]; then
  alias diff="colordiff -u"
else
  alias diff='diff -u'
fi

## git
if [[ -x "$(which hub)" ]]; then
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
alias -g B="| bat"
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

## less
alias less="less -q -g -R -j 10 -M"
export LESSCHARSET=utf-8
export LESS_TERMCAP_mb=$'\E[01;33m'    # begin blinking
export LESS_TERMCAP_md=$'\E[01;33m'    # begin bold (bold, bright yellow)
export LESS_TERMCAP_me=$'\E[0m'        # end mode
export LESS_TERMCAP_se=$'\E[0m'        # end standout-mode
export LESS_TERMCAP_so=$'\E[0;37;44m'  # begin standout-mode (white on blue)
export LESS_TERMCAP_ue=$'\E[0m'        # end underline
export LESS_TERMCAP_us=$'\E[04;36m'    # begin underline - (underline, cyan)

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
if [[ -n ~/.zcompdump(\#qN.mh+24) ]]; then
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

export HISTFILE=~/.zsh/history
export HISTSIZE=100000
export SAVEHIST=100000

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

zstyle ':chpwd:*' recent-dirs-default true
zstyle ':chpwd:*' recent-dirs-max 500
zstyle ':chpwd:*' recent-dirs-file "$HOME/.zsh/chpwd-recent-dirs"
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
export FZF_DEFAULT_OPTS="--height 14 --reverse"

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

# search from git ls-files
fzf-git-ls-files() {
  _is_in_git_repo || return
  local -a files=($(git ls-files | fzf -m))
  local ret=$?
  LBUFFER+=$files
  zle reset-prompt
  return $ret
}
zle -N fzf-git-ls-files
bindkey '^x^f' fzf-git-ls-files

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
bindkey '^x^v' fzf-git-untracked-or-changed-files

# git checkout to selected branches in recent used
fzf-git-checkout-recent-branch() {
  _is_in_git_repo || return
  local branch="$(git for-each-ref --format='%(refname:short)' --sort=-committerdate refs/heads | fzf +m -e --tiebreak=index)"
  if [[ -z "$branch" ]]; then
     zle redisplay
     return 0
  fi
  BUFFER="git checkout $branch"
  zle reset-prompt
  zle accept-line
}
zle -N fzf-git-checkout-recent-branch
bindkey '^x^b' fzf-git-checkout-recent-branch

# jump to directory selected from ghq / cdr
fzf-jump-ghq-cdr() {
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
zle -N fzf-jump-ghq-cdr
bindkey '^j' fzf-jump-ghq-cdr

# search strings like a file name displayed in current tmux pane
fzf-search-tmux-pane-strings() {
  local -a strings=($(tmux capture-pane -p | perl -pe 's/\s+/\n/g; s/\Q...\E/\n/g' | \grep -E '[a-zA-Z_\-\/]{4,}' | \grep -v '(git:' | tac))
  local str="$(print -l ${(u)strings} | fzf +m -e --tiebreak=index)"
  if [[ -z "$str" ]]; then
    zle redisplay
    return 0
  fi
  LBUFFER+="$str"
  zle reset-prompt
}
zle -N fzf-search-tmux-pane-strings
bindkey '^o' fzf-search-tmux-pane-strings

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

# display perl module's version
pmver() {
  readonly pkg="$1"
  perl -m${pkg} -E "say \$${pkg}::VERSION"
}

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

## zsh-syntax-highlighting
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
ZSH_HIGHLIGHT_STYLES[unknown-token]=fg:red,underline
ZSH_HIGHLIGHT_STYLES[path]=none
ZSH_HIGHLIGHT_STYLES[path_approx]=none
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)

## compile
if [ ! -f "~/.zshrc.zwc" -o "~/.zshrc" -nt "~/.zshrc.zwc" ]; then
  zcompile ~/.zshrc
fi

########################################
## Profiling by zprof
########################################
if [[ -x "$(which zprof)" ]]; then
  zprof | less
fi
