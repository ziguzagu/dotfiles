## -*- mode: makefile-gmake; -*-
home    = $(HOME)/
basedir = $(dir $(realpath $(lastword $(MAKEFILE_LIST))))
sources = $(shell git ls-files | grep -Ev '^(Makefile|\.gitmodules)$$')
targets = $(addprefix $(home),$(sources))
screen256color = $(HOME)/.terminfo/73/screen-256color

all: $(targets) $(screen256color)

$(targets):
	@mkdir -m 700 -p $(dir $@)
	ln -s $(subst $(home),$(basedir),$@) $@

# update OS X's default screen-256color terminfo to support underline (mainly for emacs)
#   see: http://superuser.com/questions/529655/correct-way-to-get-emacs-16-color-support-inside-tmux
$(screen256color):
	{ infocmp -x screen-256color; printf '\t%s\n' 'ncv@,'; } > /tmp/t && tic -x /tmp/t
