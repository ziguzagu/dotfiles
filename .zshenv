# -*- mode: shell-script; -*-

export XDG_DATA_HOME=$HOME/.local/share

export LANG=en_US.UTF-8

export PATH=$HOME/.local/bin:$PATH

export PAGER=less

# Set EDITOR to avoid infinite loop when running git commit in vterm
if [[ -n "$INSIDE_EMACS" ]]; then
  export EDITOR="emacsclient"
else
  export EDITOR="emacsclient -t"
fi
export ALTERNATE_EDITOR=vi
