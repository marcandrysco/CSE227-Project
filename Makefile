all: main

main:
	ghc -isrc --make -O2 src/Main.hs

deps:
	cabal install binary binary-bits bytestring quickcheck scotty wai-extra
