home    = $(HOME)/
basedir = $(dir $(realpath $(lastword $(MAKEFILE_LIST))))
sources = $(shell git ls-files | grep -Ev '^(Makefile|\.gitmodules)$$')
targets = $(addprefix $(home),$(sources))

all: $(targets)

$(targets):
	@mkdir -m 700 -p $(dir $@)
	ln -s $(subst $(home),$(basedir),$@) $@
