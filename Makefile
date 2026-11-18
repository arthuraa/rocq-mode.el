EMACS ?= emacs
SOURCES = rocq-syntax.el rocq-indent.el rocq-mode.el

.PHONY: compile test clean

compile:
	$(EMACS) --batch -Q -L . --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(SOURCES)

test:
	$(EMACS) --batch -Q -L . -l test/rocq-mode-tests.el \
	  -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc
