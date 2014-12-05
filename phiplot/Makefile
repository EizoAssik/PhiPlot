vpath %.hs src
vpath %.hs test

GHC=ghc
LIBGMP=/usr/local/lib

phicc: Structure.hs Lexer.hs Parser.hs Compiler.hs Assembler.hs Preprocessor.hs Phi.hs
	$(GHC) -o $@ -L$(LIBGMP) $^ 

compiler: Structure.hs Lexer.hs Parser.hs Compiler.hs test/TCompiler.hs Preprocessor.hs test/TCommon.hs
	$(GHC) -o $@ -L$(LIBGMP) $^ 

parser: Structure.hs Lexer.hs Parser.hs test/TParser.hs test/TCommon.hs
	$(GHC) -o $@ -L$(LIBGMP) $^ 

lexer: Structure.hs Lexer.hs test/TLexer.hs test/TCommon.hs
	$(GHC) -o $@ -L$(LIBGMP) $^ 

clean:
	rm -f src/*.hi src/*.o src/*~ lexer
