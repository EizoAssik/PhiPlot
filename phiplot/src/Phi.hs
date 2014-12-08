module Main where

import Lexer ( lexme )
import Parser ( parse )
import Compiler ( compile )
import Optimizer ( opt_ast )
import Preprocessor ( alloc, trans_jmp, trans_fcall )
import Assembler
import Data.ByteString ( pack, writeFile )
import System.Environment ( getArgs )

main = do
    args <- getArgs
    let fn = head args
        hexfn = fn ++ ".hex"
    src <- readFile fn
    let tokens = lexme src
        ast = Parser.parse tokens 
        code_row = compile $ opt_ast ast
        code = trans_fcall $ trans_jmp $ alloc code_row
        hex = pack $ foldl1 (++) $ map word64ToWord8 $ asm code
    Data.ByteString.writeFile hexfn hex


