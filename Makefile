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

# Homebrew refuses to load formulae from third-party taps until they are trusted.
# While untrusted, it cannot see emacs-plus's dependency graph, so every library
# only Emacs needs looks unused and gets swept away by bundle cleanup/autoremove.
trusted_formulae = d12frosted/emacs-plus/emacs-plus@31

brew-trust: ## Trust third-party tap formulae required by Brewfile
	brew trust --formula $(trusted_formulae)

brew: ## Update homebrew stuff
	brew update --verbose
	$(MAKE) brew-trust
	brew bundle
	brew bundle check --verbose
	brew cleanup

keyrepeat: ## Set my best key repeat settings
	defaults write -g InitialKeyRepeat -int 11
	defaults write -g KeyRepeat -int 1

# Workaround for Emacs.app/emacsclient not launched from within Ghostty (e.g. GUI app, daemon), which otherwise fail to resolve TERM=xterm-ghostty
ghostty-terminfo: ## Register Ghostty's terminfo entry into ~/.terminfo so it resolves without tmux
	infocmp -x xterm-ghostty | tic -x -o $(HOME)/.terminfo -

claude: ## Merge base Claude Code settings into ~/.claude/settings.json
	@mkdir -p $(HOME)/.claude
	@base=$(basedir).config/claude/settings.base.json; \
	if [ -f $(HOME)/.claude/settings.json ]; then \
		jq -s '.[0] as $$orig | .[1] as $$base | $$orig \
			| .permissions.allow = ((.permissions.allow // []) + ($$base.permissions.allow // []) | unique) \
			| .hooks.Notification = ((.hooks.Notification // []) + ($$base.hooks.Notification // []) | unique) \
			| .statusLine = ($$base.statusLine // .statusLine)' \
			$(HOME)/.claude/settings.json $$base > $(HOME)/.claude/settings.json.tmp && \
		mv $(HOME)/.claude/settings.json.tmp $(HOME)/.claude/settings.json; \
	else \
		jq . $$base > $(HOME)/.claude/settings.json; \
	fi
	@echo "Claude Code settings merged."

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
