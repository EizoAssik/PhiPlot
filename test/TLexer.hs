module Main where

import Lexer ( lexme )
import System.Environment ( getArgs )
import TCommon ( printListLn, format_token )


main = do
    args <- getArgs
    let fn = head args
    srcs <- readFile $ head args
    let tokens = lexme srcs
        tkstr  = reverse $ format_token tokens []
    printListLn tkstr

