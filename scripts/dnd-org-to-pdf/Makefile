.PHONY: build all run nix nix-run format test build-test

GHC_OPTIONS = -threaded -odir out -hidir out -o out/main -isrc
GHC_OPTIONS+= -Wall
GHC_OPTIONS+= -Wno-name-shadowing
GHC_OPTIONS+= -Werror=incomplete-patterns

SRCDIR=src
OUTDIR=out

nix:
	nix-build default.nix

build:
	mkdir -p ${OUTDIR}
	ghc -O2 ${GHC_OPTIONS} Main

build-fast:
	mkdir -p ${OUTDIR}
	ghc ${GHC_OPTIONS} Main

run: build
	./${OUTDIR}/main

run-fast: build-fast
	./${OUTDIR}/main

spec: build-spec
	./${OUTDIR}/spec

build-spec:
	mkdir -p ${OUTDIR}
	ghc ${GHC_OPTIONS} -o ${OUTDIR}/spec -odir ${OUTDIR} -hidir ${OUTDIR} --make ${SRCDIR}/Spec.hs -i${SRCDIR} -main-is Spec.main

clean:
	rm -rf ./out

nix-run: nix
	./result/bin/battery-status

format:
	nix-shell --pure --run "find . -type f -name \*.hs -exec brittany --write-mode=inplace {} ';'" nix/formatshell.nix
