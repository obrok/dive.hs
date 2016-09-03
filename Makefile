.PHONY: all lint run install

all: install lint run

install:
	cabal install

lint:
	.cabal-sandbox/bin/hlint .

run:
	cabal run
