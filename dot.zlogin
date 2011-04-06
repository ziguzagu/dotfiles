## -*- mode: shell-script; -*-

which keychain >& /dev/null || return

for key in id 6a sakk
do
    test -f $HOME/.ssh/$key && keychain $HOME/.ssh/$key
done

source ~/.keychain/$HOST-sh
