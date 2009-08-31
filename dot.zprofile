## -*- mode: shell-script; -*-

## lang/locale
export LANGUAGE=ja_JP:ja:en_GB:en
export LC_ALL=C
export LANG=ja_JP.UTF-8

export SHELL=`which zsh`

## path
MYPATH=~/bin
[ -d ~/opt/bin ]      && MYPATH=$MYPATH:~/opt/bin
[ -d ~/opt/flex/bin ] && MYPATH=$MYPATH:~/opt/flex/bin
[ -d /var/lib/gems/1.8/bin ] && MYPATH=$MYPATH:/var/lib/gems/1.8/bin
[ -d /opt/local/bin ] && MYPATH=$MYPATH:/opt/local/bin
export PATH=$MYPATH:$PATH
[ -d /opt/local/man ] && export MANPATH=/opt/local/man:$MANPATH
# for sakura
[ -d ~/MailBox/$USER ] && export MAILPATH=~/MailBox/$USER/maildir
[ -d ~/opt/cpan-lib ]  && export PERL5LIB=~/opt/cpan-lib:$PERL5LIB

## pager
which lv >& /dev/null && export PAGER=lv
export GIT_PAGER=cat

## editor
export EDITOR=emacscliet
export ALTERNATE_EDITO=vi
export SVN_EDITOR=emacsclient

## my
export REPOS=http://code.norainu.net/svn

## grep
export GREP_COLOR='07;33'
export GREP_OPTIONS='--exclude=\*.svn\*'

## local::lib
eval $(perl -Iperl5/lib/perl5 -Mlocal::lib 2>/dev/null)

## changing title of screen's window by preexec()
if [[ -n $WINDOW ]]; then
#    chpwd () { echo -n "_`dirs`\\" }
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
#    chpwd
fi

## change the terminal window title
#precmd() { echo -n "\e]0;$USER@$HOST\a" }

## at sixapart
[ -f .zprofile.6a ] && source .zprofile.6a
