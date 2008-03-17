## -*- mode: shell-script; -*-
## $Id$

which keychain >& /dev/null && keychain ~/.ssh/id_rsa && source ~/.keychain/$HOST-sh
