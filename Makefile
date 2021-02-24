## -*- mode: makefile-gmake; -*-
home    = $(HOME)/
basedir = $(dir $(realpath $(lastword $(MAKEFILE_LIST))))
sources = $(shell git ls-files | grep -Ev '(Makefile|Brewfile|\.terminfo|\.md|\.txt)$$')
targets = $(addprefix $(home),$(sources))

.DEFAULT_GOAL := help

install: $(targets) ## Install dot files into $HOME as symlink
	ln -sf ~/Dropbox/Library/aspell/aspell.en.pws ~/.aspell.en.pws
	ln -sf ~/Dropbox/Library/aspell/aspell.en.prepl ~/.aspell.en.prepl
	@mkdir -p $(XDG_DATA_HOME)/{tig,zsh,terminfo}

$(targets):
	@mkdir -m 700 -p $(dir $@)
	ln -sf $(subst $(home),$(basedir),$@) $@

terminfo: ## Install extra terminfo missing of macOS 10.15
	tic -x xterm.terminfo
	tic -x tmux.terminfo

brew: ## Update homebrew stuff
	brew update --verbose
	brew bundle --no-lock
	brew bundle check --verbose
	brew cleanup

gems  = rubocop rubocop-performance rubocop-rspec rubocop-rails reek
gems += solargraph
gems += travis
ruby: ## Install ruby-gems of development utility
	gem update --system --no-document
	gem install --no-document --conservative $(gems)
	gem update --no-document --conservative $(gems)
	gem cleanup $(gems)

go: ## Setup Go environment
	go get golang.org/x/tools/gopls
	go get github.com/motemen/gore/cmd/gore
	go get github.com/mdempsky/gocode
	go get github.com/k0kubun/pp

keyrepeat: ## Set my best key repeat settings
	defaults write -g InitialKeyRepeat -int 15
	defaults write -g KeyRepeat -int 1

rainbow: ## Test terminal's 24-bit color support
	curl -s https://raw.githubusercontent.com/gnachman/iTerm2/master/tests/24-bit-color.sh | bash

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
