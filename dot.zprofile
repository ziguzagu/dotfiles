## -*- mode: shell-script; -*-

## lang/locale
export LANGUAGE=ja_JP:ja:en_GB:en
export LC_ALL=C
export LANG=ja_JP.UTF-8

export SHELL=`which zsh`

## path/manpath
# sakura
[ -d $HOME/MailBox/$USER ] && export MAILPATH=~/MailBox/$USER/maildir
[ -d $HOME/opt/bin ] && export PATH=$HOME/opt/bin:$PATH
[ -d $HOME/opt/man ] && export MANPATH=$HOME/opt/man:$MANPATH
# mac
[ -d /usr/local/git/man ] && export MANPATH=/usr/local/git/man:$MANPATH

## pager
which lv >& /dev/null && export PAGER=lv
export GIT_PAGER=cat

## editor
export EDITOR=$HOME/bin/emacsclient
export ALTERNATE_EDITOR=vi

## grep
export GREP_COLOR='07;33'
export GREP_OPTIONS='--exclude=\*.svn\*'

## trying to use perlbrew or local::lib
if [ -f ~/perl5/perlbrew/etc/bashrc ]; then
    source ~/perl5/perlbrew/etc/bashrc
elif [ -n $PERL5LIB ]; then
    eval $(perl -Iperl5/lib/perl5 -Mlocal::lib 2>/dev/null)
fi

## golang
export GOROOT=$HOME/go
export GOOS=darwin
export GOARCH=amd64

## gisty
export GISTY_DIR=$HOME/dev/gists

## changing title of screen's window by preexec()
if [[ -n $WINDOW ]]; then
    preexec() {
        emulate -L zsh
        local -a cmd; cmd=(${(z)2})
        case $cmd[1] in
            fg)
                if (( $#cmd == 1 )); then
                    cmd=(builtin jobs -l %+)
                else
                    cmd=(builtin jobs -l $cmd[2])
                fi
                ;;
            %*)
                cmd=(builtin jobs -l $cmd[1])
                ;;
            cd|ssh)
                if (( $#cmd == 2)); then
                    cmd[1]=$cmd[2]
                fi
                ;&
            *)
                echo -n "k$cmd[1]:t\\"
                return
                ;;
        esac

        local -A jt; jt=(${(kv)jobtexts})

        $cmd >>(read num rest
            cmd=(${(z)${(e):-\$jt$num}})
            echo -n "k$cmd[1]:t\\") 2>/dev/null
    }
fi

## change the terminal window title
#precmd() { echo -n "\e]0;$USER@$HOST\a" }

## at sixapart
[ -f .zprofile.6a ] && source .zprofile.6a
