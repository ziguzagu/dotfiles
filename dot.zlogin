## -*- mode: shell-script; -*-

## keychain
if which keychain > /dev/null; then
    keychain --quiet ~/.ssh/id ~/.ssh/sakk
    source ${HOME}/.keychain/${HOST}-sh
fi
