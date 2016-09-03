.PHONY: all lint run

all: lint run

lint:
	.cabal-sandbox/bin/hlint .

run:
	cabal run
