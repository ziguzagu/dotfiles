## $Id$

export SHELL=`which zsh`

export LANGUAGE=ja_JP:ja:en_GB:en
export LC_ALL=C
export LANG=ja_JP.UTF-8

MYPATH=~/bin
[ -d ~/opt/bin ]      && MYPATH=$MYPATH:~/opt/bin
[ -d ~/opt/flex/bin ] && MYPATH=$MYPATH:~/opt/flex/bin
export PATH=$MYPATH:$PATH

export PAGER=lv
export EDITOR=emacsclient
export SVN_EDITOR=emacsclient
export SVKMERGE=EmacsClient

export REPOS=http://code.norainu.net/svn/

## for sakura
[ -d ~/MailBox/$USER ] && export MAILPATH=~/MailBox/$USER/maildir
[ -d ~/opt/cpan-lib ]  && export PERL5LIB=~/opt/cpan-lib:$PERL5LIB

## colorization for grep
if [ `uname` = "Linux" ]; then
    export GREP_COLOR='07;33'
    export GREP_OPTIONS="--binary-files=without-match --color=always"
fi

## changing title of screen's window by preexec()
if [[ -n $WINDOW ]]; then
    chpwd () { echo -n "_`dirs`\\" }
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
            cd)
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
    chpwd
fi

## in sixapart
[ -f .zshenv.6a ] && source .zshenv.6a
