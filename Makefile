cwd = $(shell pwd)
sources = $(shell git ls-files | grep -Ev '^(Makefile|\.gitmodules)$$')
targets = $(addprefix $(HOME)/,$(sources))

all: $(targets)

$(targets):
	@mkdir -m 700 -p $(dir $@)
	ln -s $(subst $(HOME),$(cwd),$@) $@
