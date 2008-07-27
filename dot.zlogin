## -*- mode: shell-script; -*-
## $Id$

which keychain >& /dev/null || return

for key in $HOME/.ssh/id_rsa $HOME/.ssh/id_rsa-6a
do
    test -f $key && keychain $key
done

source ~/.keychain/$HOST-sh
