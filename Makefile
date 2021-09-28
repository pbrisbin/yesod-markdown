all: setup build test lint

.PHONY: clean
clean:
	stack clean

.PHONY: setup
setup:
	stack setup
	stack build --dependencies-only --test --no-run-tests

.PHONY: setup.lint
setup.lint:
	stack install --copy-compiler-tool hlint weeder

.PHONY: build
build:
	stack build --fast --pedantic --test --no-run-tests

.PHONY: test
test:
	stack build --fast --pedantic --test

.PHONY: lint
lint:
	stack exec hlint .
	stack exec weeder .

.PHONY: nightly
nightly:
	stack setup --stack-yaml stack-nightly.yaml --resolver nightly
	stack build --stack-yaml stack-nightly.yaml --resolver nightly \
	  --test --no-run-tests --bench --no-run-benchmarks \
	  --dependencies-only
	stack build --stack-yaml stack-nightly.yaml --resolver nightly \
	  --test --bench --no-run-benchmarks \
	  --fast --pedantic
