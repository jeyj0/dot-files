.PHONY: build

GHC_OPTIONS = -threaded -odir out -hidir out -o out/main -isrc
GHC_OPTIONS+= -Wall
GHC_OPTIONS+= -Werror=incomplete-patterns

build:
	mkdir -p out
	ghc -O2 ${GHC_OPTIONS} Main

