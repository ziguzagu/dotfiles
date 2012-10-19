## -*- mode: shell-script; -*-

## keychain
if which keychain > /dev/null; then
    keychain --quiet ~/.ssh/id ~/.ssh/sakk
    source ~/.keychain/${HOST}-sh
fi
