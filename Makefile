.PHONY: build format lint test run clean refactor repl

all: format build test

build: 
	@stack build

format:
	@stylish-haskell -i src/*.hs
	@stylish-haskell -i app/*.hs

lint:
	@hlint src/*.hs app/*.hs

refactor:
	@hlint --refactor --refactor-options="-i" src/*.hs
	@hlint --refactor --refactor-options="-i" app/*.hs

test:
	@stack test

run:
	@stack run -- +RTS -N4

clean:
	@rm -rf .stack-work kmeans.cabal

repl:
	@stack repl
