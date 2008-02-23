## -*- mode: shell-script; -*-
## $Id$

export LANGUAGE=ja_JP:ja:en_GB:en
export LC_ALL=C
export LANG=ja_JP.UTF-8

## at sakura
[ -d ~/MailBox/$USER ] && export MAILPATH=~/MailBox/$USER/maildir
[ -d ~/opt/cpan-lib ]  && export PERL5LIB=~/opt/cpan-lib:$PERL5LIB

## at sixapart
[ -f .zshenv.6a ] && source .zshenv.6a
