EMACS = emacs -Q --batch

.PHONY: setup lint test clean

all: test

setup:
	cask install

lint:
	cask $(EMACS) -l elisp-lint.el -f elisp-lint-files-batch finkel-mode.el

test:
	cask exec buttercup -L .

clean:
	rm -f *~ *.elc *-autoloads.el test/*~ test/*.elc test/*-autoloads.el
