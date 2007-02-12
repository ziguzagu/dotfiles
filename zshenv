## $Id$

export SHELL=`which zsh`

export LANGUAGE=ja_JP:ja:en_GB:en
export LC_ALL=C
export LANG=ja_JP.UTF-8

export PAGER=lv
export EDITOR=vi
export SVN_EDITOR=vi

export PATH=~/bin:$PATH

export REPOS=http://code.norainu.net/svn/

export GREP_COLOR='07;33'
export GREP_OPTIONS="--binary-files=without-match --color=always"

## changing title of screen's window by preexec()
if [ "$WINDOW" != "" ]; then
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
                ;;
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
if [ -e .sixapartenv ]; then
    source .sixapartenv
fi
