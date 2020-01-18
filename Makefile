## -*- mode: makefile-gmake; -*-
home    = $(HOME)/
basedir = $(dir $(realpath $(lastword $(MAKEFILE_LIST))))
sources = $(shell git ls-files | grep -Ev '(README\.md|Makefile|Brewfile|\.terminfo)$$')
targets = $(addprefix $(home),$(sources))

.DEFAULT_GOAL := help

install: $(targets) ## Install dot files into $HOME as symlink

$(targets):
	@mkdir -m 700 -p $(dir $@)
	ln -sf $(subst $(home),$(basedir),$@) $@

terminfo: ## Install extra terminfo missing of OS X 10.11
	tic -x xterm.terminfo
	tic -x tmux.terminfo

brew: ## Update homebrew stuff
	brew update
	brew bundle --no-lock
	brew bundle check --verbose
	brew cleanup

keyrepeat:
	defaults write -g InitialKeyRepeat -int 15
	defaults write -g KeyRepeat -int 1

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
