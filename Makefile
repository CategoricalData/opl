build:
	cabal build
clean:
	cabal clean
configure:
	cabal configure
run:
	./dist/build/opl/opl opl_source/sample.opl
interact:
	ghci -isrc src/Lang/OPL/Main.hs
