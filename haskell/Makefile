all: test

build:
	mkdir -p bin obj
	ghc --make -Wall -threaded -o bin/tetris -outputdir obj -iHUnit-1.0 *.hs

clean:
	rm -r bin obj

test: build
	bin/tetris +RTS -N4
