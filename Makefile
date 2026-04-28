EMACS ?= emacs
ELPA_DIR := .deps
PACKAGE := tada-list.el
TESTS := test/tada-list-tests.el

INIT := --eval "(setq package-user-dir (expand-file-name \"$(ELPA_DIR)\"))" \
        --eval "(require 'package)" \
        --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
        --eval "(package-initialize)"

.PHONY: all check compile lint test deps clean

all: check

check: lint compile test

deps:
	@$(EMACS) --batch $(INIT) \
		--eval "(unless (package-installed-p 'package-lint) \
		          (package-refresh-contents) \
		          (package-install 'package-lint))"

compile:
	@$(EMACS) --batch -L . -f batch-byte-compile $(PACKAGE)

lint: deps
	@$(EMACS) --batch \
		--eval "(checkdoc-file \"$(PACKAGE)\")"
	@$(EMACS) --batch $(INIT) -L . \
		-l package-lint \
		-f package-lint-batch-and-exit \
		$(PACKAGE)

test:
	@$(EMACS) --batch -L . -L test \
		-l ert -l $(TESTS) \
		-f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc
	rm -rf $(ELPA_DIR)
