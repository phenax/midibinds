
hpack:
	hpack

run: hpack
	cabal run . -- "MPK Mini Mk II MIDI 1"

build: hpack
	cabal build


