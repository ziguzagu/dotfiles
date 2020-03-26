## -*- mode: makefile-gmake; -*-
home    = $(HOME)/
basedir = $(dir $(realpath $(lastword $(MAKEFILE_LIST))))
sources = $(shell git ls-files | grep -Ev '(Makefile|Brewfile|\.terminfo|\.md|\.txt)$$')
targets = $(addprefix $(home),$(sources))

.DEFAULT_GOAL := help

install: $(targets) ## Install dot files into $HOME as symlink
	ln -sf $(HOME)/Dropbox/Library/aspell/aspell.en.pws $(HOME)/.aspell.en.pws
	ln -sf $(HOME)/Dropbox/Library/aspell/aspell.en.prepl $(HOME)/.aspell.en.prepl

$(targets):
	@mkdir -m 700 -p $(dir $@)
	ln -sf $(subst $(home),$(basedir),$@) $@

terminfo: ## Install extra terminfo missing of macOS 10.15
	tic -x xterm.terminfo
	tic -x tmux.terminfo

brew: ## Update homebrew stuff
	brew update
	brew bundle --no-lock
	brew bundle check --verbose
	brew cleanup

gems  = rubocop rubocop-performance rubocop-rspec rubocop-rails reek
gems += solargraph
gems += travis
ruby: ## Install ruby-gems of development utility
	gem update --system --no-document
	gem install --no-document --conservative $(gems)
	gem cleanup $(gems)

keyrepeat: ## Set my best key repeat settings
	defaults write -g InitialKeyRepeat -int 15
	defaults write -g KeyRepeat -int 1

rainbow: ## Test terminal's 24-bit color support
	curl -s https://raw.githubusercontent.com/gnachman/iTerm2/master/tests/24-bit-color.sh | bash

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
