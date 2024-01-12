## -*- mode: makefile-gmake; -*-
SHELL = /bin/zsh

home    = $(HOME)/
datadir = $(HOME)/.local/share
basedir = $(dir $(realpath $(lastword $(MAKEFILE_LIST))))
sources = $(shell cat <(git ls-files) <(git ls-files -ic -X .install-ignore) | sort | uniq -u)
targets = $(addprefix $(home),$(sources))

.DEFAULT_GOAL := help

install: $(targets) --dropbox ## Install dot files into $HOME as symlink
	@mkdir -p $(datadir)/{tig,zsh,terminfo}

$(targets):
	@mkdir -m 700 -p $(dir $@)
	ln -s $(subst $(home),$(basedir),$@) $@

--dropbox: ~/.aspell.en.pws ~/.aspell.en.prepl
~/.aspell.en.pws: ~/Dropbox/Library/aspell/aspell.en.pws
~/.aspell.en.prepl: ~/Dropbox/Library/aspell/aspell.en.prepl
	ln -s $< $@

terminfo: ## Install extra terminfo missing of macOS
	tic -x 24bit.terminfo

brew: ## Update homebrew stuff
	brew update --verbose
	brew bundle --no-lock
	brew bundle check --verbose
	brew cleanup

go: ## Update local development environment of Go
	go get golang.org/x/tools/gopls
	go get github.com/motemen/gore/cmd/gore
	go get github.com/mdempsky/gocode
	go get github.com/k0kubun/pp

keyrepeat: ## Set my best key repeat settings
	defaults write -g InitialKeyRepeat -int 11
	defaults write -g KeyRepeat -int 1

rainbow: ## Test terminal's 24-bit color support
	curl -s https://raw.githubusercontent.com/gnachman/iTerm2/master/tests/24-bit-color.sh | bash

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
