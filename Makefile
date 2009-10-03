all:
	mkdir bin
	ghc --make -o bin/tetris -iHUnit-1.0 tetris.hs
