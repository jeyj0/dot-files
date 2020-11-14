.PHONY: build all run nix nix-run format

GHC_OPTIONS = -threaded -odir out -hidir out -o out/main -isrc
GHC_OPTIONS+= -Wall
GHC_OPTIONS+= -Werror=incomplete-patterns

nix:
	nix-build default.nix

build:
	mkdir -p out
	ghc -O2 ${GHC_OPTIONS} Main

build-fast:
	mkdir -p out
	ghc ${GHC_OPTIONS} Main

run: build
	./out/main

clean:
	rm -rf ./out

nix-run: nix
	./result/bin/battery-status

format:
	nix-shell --pure --run "find . -type f -name \*.hs -exec brittany --write-mode=inplace {} ';'" nix/formatshell.nix
