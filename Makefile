all: setup build test lint

.PHONY: setup
setup:
	stack setup $(STACK_ARGUMENTS) --no-terminal
	# Avoid ExitFailure (-9) (THIS MAY INDICATE OUT OF MEMORY)
	stack build $(STACK_ARGUMENTS) --dependencies-only --test --no-run-tests --no-terminal -j 1 Cabal haskell-src-exts
	stack build $(STACK_ARGUMENTS) --dependencies-only --test --no-run-tests --no-terminal
	stack install $(STACK_ARGUMENTS) hlint weeder

.PHONY: build
build:
	stack build $(STACK_ARGUMENTS) --pedantic --test --no-run-tests

.PHONY: test
test:
	stack build $(STACK_ARGUMENTS) --pedantic --test


.PHONY: lint
lint:
	hlint .
	weeder .
