vpath %.hs src

GHC=ghc
LIBGMP=/usr/local/lib

parser: Lexer.hs Parser.hs TParser.hs
	$(GHC) -o $@ -L$(LIBGMP) $^ 

lexer: Lexer.hs TLexer.hs
	$(GHC) -o $@ -L$(LIBGMP) $^ 

clean:
	rm -f src/*.hi src/*.o src/*~ lexer
