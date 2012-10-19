## -*- mode: shell-script; -*-

which keychain >& /dev/null || return

keychain --quiet ~/.ssh/id ~/.ssh/sakk
test -f ~/.keychain/$HOST-sh && source ~/.keychain/$HOST-sh
