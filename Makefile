
hpack:
	hpack

run: hpack
	cabal run . -- connect "MPK Mini Mk II MIDI 1"

debug: hpack
	cabal run . -- debug "MPK Mini Mk II MIDI 1"

build: hpack build-simple

build-simple:
	cabal build

install: build-simple
	cabal install

