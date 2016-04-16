## -*- mode: shell-script; -*-

## keychain
if which keychain > /dev/null; then
    keychain --quiet ~/.ssh/id_rsa
    source ~/.keychain/${HOST}-sh
fi
