module Main where

import Lexer ( lexme )
import Parser ( parse )
import Compiler ( compile )
import Preprocessor ( alloc, trans_jmp )
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
        code_row = foldl1 (++) $ map compile ast
        code = trans_jmp $ alloc code_row
        hex = pack $ foldl1 (++) $ map word64ToWord8 $ asm code
    Data.ByteString.writeFile hexfn hex


