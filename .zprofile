PATH="/usr/local/bin:/usr/local/sbin:$PATH"

## using coreutils on mac installed by homebrew
if which brew > /dev/null; then
    PATH="$(brew --prefix coreutils)/libexec/gnubin:/usr/local/share/npm/bin:$PATH"
fi
## setup perl env, trying to use plenv then local::lib
if [ -d "$HOME/.plenv" ]; then
    PATH="${HOME}/.plenv/bin:${PATH}"
fi
## rbenv
if [ -d "$HOME/.rbenv" ]; then
    PATH="${HOME}/.rbenv/bin:${PATH}"
fi
## pyenv
if [ -d "$HOME/.pyenv" ]; then
    PATH="${HOME}/.pyenv/bin:${PATH}"
fi
## my script
if [ -d "$HOME/bin" ]; then
    PATH="$HOME/bin:$PATH"
fi

MANPATH="/usr/local/share/man:$MANPATH"

## removed duplicated entries
typeset -U PATH MANPATH
export PATH MANPATH
