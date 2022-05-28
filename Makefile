
hpack:
	hpack

run: hpack
	cabal run . -- "MPK Mini Mk II MIDI 1"

build: hpack build-simple

build-simple:
	cabal build

install: build-simple
	cabal install

