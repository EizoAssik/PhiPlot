vpath %.hs src
vpath %.hs test

GHC=ghc
LIBGMP=/usr/local/lib

parser: Structure.hs Lexer.hs Parser.hs test/TParser.hs test/TCommon.hs
	$(GHC) -o $@ -L$(LIBGMP) $^ 

lexer: Structure.hs Lexer.hs test/TLexer.hs test/TCommon.hs
	$(GHC) -o $@ -L$(LIBGMP) $^ 

clean:
	rm -f src/*.hi src/*.o src/*~ lexer
