all: setup build test lint

.PHONY: setup
setup:
	stack setup $(STACK_ARGUMENTS) --no-terminal
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
