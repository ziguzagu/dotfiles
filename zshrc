## $Id$

#### alias
alias ls="ls -F"
alias l="ls -l"
alias ll="ls -al"
alias sc="screen -xRU"
alias gv="grep -v .svn"
alias lv="lv -c"
alias vs="svn status -u"
alias vd="svn diff"
alias vl="svn log --stop-on-copy -v"
alias us="TZ=US/Pacific date"
alias push="svk push --verbatim"
## global alias
alias -g M="| more"
alias -g L="| lv"
alias -g G="| grep"
alias -g GV="| grep -v"

#### bindkey
bindkey -e

#### completion
autoload -U compinit
compinit
setopt auto_cd
setopt auto_name_dirs
setopt auto_remove_slash
setopt extended_glob
setopt auto_param_keys
setopt auto_list
setopt auto_menu
setopt list_types
setopt cdable_vars

#### history
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

#### color terminal
eval `tset -sQI xterm-256color`

#### prompt
setopt prompt_subst
## for emacs (no escape usging)
if [ "$TERM" = "dumb" ]; then
    PROMPT='[%n@%m]%# '
    RPROMPT='[%(5~,%-2~/.../%2~,%~)]'
else
    PROMPT='%{[36m%}[%n@%m]%{[m%}%# '
    RPROMPT='%{[33m%}[%(5~,%-2~/.../%2~,%~)]%{[m%}'
fi

#### misc
setopt correct
setopt no_hup
setopt no_beep
setopt sun_keyboard_hack
setopt always_last_prompt
setopt sh_word_split
setopt no_flow_control

#### limit
limit coredumpsize 0

#### ssh_screen
function ssh_screen(){
    eval server=\${$#}
    screen -t $server ssh "$@"
}
if [ "$WINDOW" != "" ]; then
    alias ssh=ssh_screen
fi

#### dabbrev on screen
HARDCOPYFILE=$HOME/.screen-hardcopy
touch $HARDCOPYFILE

dabbrev-complete () {
    local reply lines=80
    screen -X eval "hardcopy -h $HARDCOPYFILE"
    reply=($(sed '/^$/d' $HARDCOPYFILE | sed '$ d' | tail -$lines))
    compadd - "${reply[@]%[*/=@|]}"
}

zle -C dabbrev-complete menu-complete dabbrev-complete
bindkey '^o' dabbrev-complete
bindkey '^o^_' reverse-menu-complete

#### href (haskell api documents) complete
compctl -K _href href
functions _href () {
    reply=(`cat /usr/share/href/comptable|awk -F, '{print $2}'|sort|uniq`)
}

#### move parent directory with '^'.
function cdup() {
    echo
    cd ..
    zle reset-prompt
}
zle -N cdup
bindkey '\^' cdup

#### in sixapart
if [ -e .sixapartrc ]; then
    source .sixapartrc
fi
