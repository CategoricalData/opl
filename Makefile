build:
	cabal build
clean:
	cabal clean
configure:
	cabal configure
run:
	./dist/build/opl/opl
interact:
	ghci -isrc src/Lang/OPL/Main.hs
