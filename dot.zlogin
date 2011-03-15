## -*- mode: shell-script; -*-

which keychain >& /dev/null || return

for key in $HOME/.ssh/id $HOME/.ssh/6a $HOME/.ssh/sakk
do
    test -f $key && keychain $key
done

source ~/.keychain/$HOST-sh
