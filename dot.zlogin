## -*- mode: shell-script; -*-

which keychain >& /dev/null || return

for key in id sakk
do
    test -f $HOME/.ssh/$key && keychain --quiet --quick --nolock --noask $HOME/.ssh/$key
done

test -f ~/.keychain/$HOST-sh && source ~/.keychain/$HOST-sh
