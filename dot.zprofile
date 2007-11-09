## $Id$

MYPATH=~/bin
[ -d ~/opt/bin ]      && MYPATH=$MYPATH:~/opt/bin
[ -d ~/opt/flex/bin ] && MYPATH=$MYPATH:~/opt/flex/bin
[ -d ~/opt/gems/bin ] && MYPATH=$MYPATH:~/opt/gems/bin
export PATH=$MYPATH:$PATH

export SHELL=`which zsh`

export PAGER=lv
export EDITOR=emacsclient
export SVN_EDITOR=emacsclient
export SVKMERGE=EmacsClient

export REPOS=http://code.norainu.net/svn/

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

## at sixapart
[ -f .zprofile.6a ] && source .zprofile.6a
