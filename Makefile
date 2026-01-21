## -*- mode: makefile-gmake; -*-
SHELL = /bin/zsh

datadir = $(HOME)/.local/share
basedir = $(dir $(realpath $(lastword $(MAKEFILE_LIST))))
sources = $(shell cat <(git ls-files) <(git ls-files -ic -X .install-ignore) | sort | uniq -u)
targets = $(addprefix $(HOME)/,$(sources))

.DEFAULT_GOAL := help

install: $(targets) ## Install dot files into $HOME as symlink
	@mkdir -p $(datadir)/{tig,zsh,ssh}

$(targets):
	@mkdir -m 700 -p $(dir $@)
	ln -s $(subst $(HOME)/,$(basedir),$@) $@

brew: ## Update homebrew stuff
	brew update --verbose
	brew bundle
	brew bundle check --verbose
	brew cleanup

keyrepeat: ## Set my best key repeat settings
	defaults write -g InitialKeyRepeat -int 11
	defaults write -g KeyRepeat -int 1

rainbow: ## Test terminal's 24-bit color support
	curl -s https://raw.githubusercontent.com/gnachman/iTerm2/master/tests/24-bit-color.sh | bash

check-deadlinks: ## Check for broken symlinks pointing to this repository
	@echo "Checking for dead symlinks pointing to $(basedir)..."; \
	deadlinks=$$(find $(HOME)/.config $(HOME)/.local -type l 2>/dev/null; \
	            find $(HOME) -maxdepth 1 -name '.*' -type l 2>/dev/null); \
	found=0; \
	for link in $${=deadlinks}; do \
		if [ ! -e "$$link" ]; then \
			target=$$(readlink "$$link"); \
			case "$$target" in \
				$(basedir)*) \
					[ $$found -eq 0 ] && echo "Found dead symlinks:"; \
					echo "  $$link -> $$target"; \
					found=1; \
					;; \
			esac; \
		fi; \
	done; \
	[ $$found -eq 0 ] && echo "No dead symlinks found"; \
	exit 0

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
