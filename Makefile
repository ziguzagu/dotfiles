## -*- mode: makefile-gmake; -*-
home    = $(HOME)/
basedir = $(dir $(realpath $(lastword $(MAKEFILE_LIST))))
sources = $(shell git ls-files | grep -Ev '(Makefile|Brewfile|\.gitmodules|\.terminfo)$$')
targets = $(addprefix $(home),$(sources))

install: $(targets) ## Install dot files into $HOME as symlink (Default)

$(targets):
	@mkdir -m 700 -p $(dir $@)
	ln -s $(subst $(home),$(basedir),$@) $@

terminfo: ## Install extra terminfo missing of OS X 10.11
	tic -x xterm.terminfo
	tic -x tmux.terminfo

update: ## Update homebrew stuff
	brew update
	brew bundle
	brew bundle check --verbose
	brew cleanup

keyrepeat:
	defaults write -g InitialKeyRepeat -int 15
	defaults write -g KeyRepeat -int 1

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
