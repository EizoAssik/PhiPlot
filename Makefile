vpath %.hs src

GHC=ghc
LIBGMP=/usr/local/lib

lexer: Lexer.hs TLexer.hs
	$(GHC) -o $@ -L$(LIBGMP) $^ 

clean:
	rm -f src/*.hi src/*.o src/*~ lexer
