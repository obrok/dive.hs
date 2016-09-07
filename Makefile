.PHONY: all lint run install configure

all: install lint configure run

install:
	cabal install

configure:
	cabal configure

lint:
	.cabal-sandbox/bin/hlint .

run:
	cabal run
