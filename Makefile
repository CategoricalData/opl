build:
	cabal build
clean:
	cabal clean
configure:
	cabal configure
run: dist/build/opl/opl
	./dist/build/opl/opl opl_source/sample.opl
dist/build/opl/opl: build
interact:
	ghci -isrc src/Lang/OPL/Main.hs
