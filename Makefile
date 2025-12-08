.PHONY: help test deps check-compile

define DEPS_SCRIPT
(progn
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)
(package-install 'buttercup)
(package-install 'request))
endef
export DEPS_SCRIPT

help:
	@echo "Available commands:"
	@echo "  make deps          Install dependencies"
	@echo "  make test          Run the tests"
	@echo "  make check-compile Check for clean byte-compilation"

deps:
	@echo "Installing dependencies"
	emacs --batch --eval "$$DEPS_SCRIPT"

test:
	emacs --batch \
	--eval "(require 'package)" \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	--eval "(package-initialize)" \
	--directory . \
	--load buttercup \
	--funcall buttercup-run-discover

check-compile: deps
	@echo "Checking byte-compilation..."
	emacs -Q --batch \
	--eval "(require 'package)" \
	--eval "(setq package-user-dir \"$(CURDIR)/.elpa\")" \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	--eval "(package-initialize)" \
	--eval "(package-install 'request)" \
	--eval "(setq byte-compile-error-on-warn t)" \
	--eval "(add-to-list 'load-path \".\")" \
	--eval "(byte-compile-file \"wiktionary-bro.el\")"
