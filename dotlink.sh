#!/bin/sh

cmd=`basename $0`
dotdir="$PWD"
dirs=`find . -type d | \grep -v .git | sed -e 's/^\.\///;/^$/d;'`
files=`find . -type f | \egrep -v '^#' | \grep -v .git/ | \grep -v $cmd | sed -e 's/^\.\///;/^$/d;'`

cd $HOME
for dir in $dirs; do
    dir=`echo $dir | sed -e 's/^dot//'`
    if [ ! -d "$dir" ]; then
        echo "mkdir $dir"
        mkdir "$dir"
    fi
done

for file in $files; do
    target=`echo $file | sed -e 's/^dot//'`
    if [ ! -L "$target" ]; then
        echo "ln -s $dotdir/$file $target"
        ln -s "$dotdir/$file" "$target"
    fi
done

exit 0
