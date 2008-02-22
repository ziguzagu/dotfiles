#!/usr/bin/env bash

CMD=`basename $0`
DOTDIR="$PWD"

DIRS=`find ./ -type d | \grep -v .svn | sed -e 's/^\.\///;/^$/d;'`
FILES=`find ./ -type f | \grep -v .svn | sed -e 's/^\.\///;/^$/d;'`
FILES=${FILES/$CMD/}

cd $HOME
for D in $DIRS; do
    D=`echo $D | sed -e 's/^dot//'`
    if [ ! -d "$D" ]; then
        mkdir "$D"
        echo "mkdir $D"
    fi
done

for F in $FILES; do
    TARGET=`echo $F | sed -e 's/^dot//'`
    if [ ! -L "$TARGET" ]; then
        ln -s "$DOTDIR/$F" "$TARGET"
        echo "ln -s $DOTDIR/$F $TARGET"
    fi
done

exit 0
