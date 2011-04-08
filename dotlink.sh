#!/bin/bash

CMD=`basename $0`
DOTDIR="$PWD"

DIRS=`find . -type d | \grep -v .git | sed -e 's/^\.\///;/^$/d;'`
FILES=`find . -type f | \grep -v .git/ | \grep -v $CMD | sed -e 's/^\.\///;/^$/d;'`

cd $HOME
for D in $DIRS; do
    D=`echo $D | sed -e 's/^dot//'`
    if [ ! -d "$D" ]; then
        echo "mkdir $D"
        mkdir "$D"
    fi
done

for F in $FILES; do
    TARGET=`echo $F | sed -e 's/^dot//'`
    if [ ! -L "$TARGET" ]; then
        echo "ln -s $DOTDIR/$F $TARGET"
        ln -s "$DOTDIR/$F" "$TARGET"
    fi
done

exit 0
