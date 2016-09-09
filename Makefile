.PHONY: all lint run install configure

all: install lint configure run

install:
	cabal install --only-dependencies

configure:
	cabal configure

lint:
	.cabal-sandbox/bin/hlint .

run:
	cabal run
