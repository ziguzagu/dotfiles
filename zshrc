## $Id$

#### alias
alias ls="ls -F"
alias l="ls -l"
alias ll="ls -al"
alias sc="screen -xRU"
alias gv="grep -v .svn"
alias lv="lv -c"
alias vs="svn status -u"
## sixapart
alias r="./tools/sixapartctl restart"
alias pg="psql -U postgres typepad-$USER"

#### bindkey
bindkey -e

#### completion
autoload -U compinit
compinit
setopt auto_cd
setopt auto_name_dirs
setopt auto_remove_slash
setopt extended_glob

#### history
HISTFILE=$HOME/.zsh-history
HISTSIZE=10000
SAVEHIST=10000
setopt extended_history
setopt hist_ignore_all_dups
setopt hist_reduce_blanks
setopt share_history
setopt hist_no_store
function history-all { history -E 1 }

#### prompt
setopt prompt_subst
PROMPT='%{[36m%}[%n@%m]%{[m%}%# '
RPROMPT='%{[33m%}[%(5~,%-2~/.../%2~,%~)]%{[m%}'

#### option
setopt auto_menu correct
setopt pushd_ignore_dups rm_star_silent sun_keyboard_hack
setopt list_types no_beep always_last_prompt
setopt cdable_vars sh_word_split auto_param_keys
setopt no_flow_control

#### limit
limit coredumpsize 0

#### ssh_screen
function ssh_screen(){
    eval server=\${$#}
    screen -t $server ssh "$@"
}
if [ x$TERM = xscreen ]; then
    alias ssh=ssh_screen
fi

#### dabbrev on screen
HARDCOPYFILE=$HOME/tmp/screen-hardcopy
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
