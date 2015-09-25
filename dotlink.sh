#!/bin/sh

cmd=`basename $0`
dotdir="$PWD"
dirs=`find . -type d | \grep -v .git | sed -e 's/^\.\///;/^$/d;'`
files=`find . -type f | \egrep -v '^#' | \grep -v .git/ | \grep -v $cmd | sed -e 's/^\.\///;/^$/d;'`

cd $HOME
for dir in $dirs; do
    if [ ! -d "$dir" ]; then
        echo "mkdir $dir"
        mkdir "$dir"
    fi
done

for file in $files; do
    if [ ! -L "$file" ]; then
        echo "ln -s $dotdir/$file $file"
        ln -s "$dotdir/$file" "$file"
    fi
done

## make .emacs.d read only for my self to write emacs-server's socket file
if [ -d ~/.emacs.d ]; then
    chmod 700 ~/.emacs.d
fi

## go-autocomplete
if [ ! -d $GOPATH/src/github.com/nsf/gocode ]; then
    eval "$(go get github.com/nsf/gocode)"
fi
if [ -f $GOPATH/src/github.com/nsf/gocode/emacs/go-autocomplete.el ] && [ ! -L ~/.emacs.d/go-autocomplete.el ]; then
    ln -s $GOPATH/src/github.com/nsf/gocode/emacs/go-autocomplete.el ~/.emacs.d/go-autocomplete.el
fi

exit 0
