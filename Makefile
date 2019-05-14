all: setup build test lint

.PHONY: clean
clean:
	stack clean

.PHONY: setup
setup:
	stack setup $(STACK_ARGUMENTS)
	# Avoid ExitFailure (-9) (THIS MAY INDICATE OUT OF MEMORY)
	stack build $(STACK_ARGUMENTS) -j 1 Cabal haskell-src-exts
	stack build $(STACK_ARGUMENTS) --dependencies-only --test --no-run-tests

.PHONY: setup.lint
setup.lint:
	stack install $(STACK_ARGUMENTS) --copy-compiler-tool hlint weeder

.PHONY: build
build:
	stack build $(STACK_ARGUMENTS) --pedantic --test --no-run-tests

.PHONY: test
test:
	stack build $(STACK_ARGUMENTS) --pedantic --test


.PHONY: lint
lint:
	stack exec $(STACK_ARGUMENTS) hlint .
	stack exec $(STACK_ARGUMENTS) weeder .

# NB. not used on CI, more for local validation
.PHONY: nightly
nightly:
	stack setup --stack-yaml stack-nightly.yaml --resolver nightly
	stack build --stack-yaml stack-nightly.yaml --resolver nightly \
	  --test --no-run-tests --bench --no-run-benchmarks \
	  --dependencies-only
	stack build --stack-yaml stack-nightly.yaml --resolver nightly \
	  --test --no-run-tests --bench --no-run-benchmarks \
	  --fast --pedantic
