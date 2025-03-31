# -*- Makefile -*-
SHELL = /bin/sh
EMACS ?= emacs

.PHONY: test solve-dependencies

EMACS_GENERIC_OPTS=--quick --directory . --directory .deps
EMACS_BATCH_OPTS:=--batch $(EMACS_GENERIC_OPTS)
RM=@rm -rf

solve-dependencies:
	@echo "Installing dependencies..."
	@mkdir -p ~/.emacs.d/lisp
	@if [ ! -d ~/.emacs.d/lisp/language-detection ]; then \
		git clone https://github.com/andreasjansson/language-detection.el ~/.emacs.d/lisp/language-detection; \
	else \
		echo "language-detection already exists, skipping..."; \
	fi
	@echo "Dependencies installed installed successfully."

test:   solve-dependencies
	@$(EMACS) $(EMACS_BATCH_OPTS) --load ./tests/html2org-tests.el
