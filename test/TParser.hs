module Main where

import Lexer
import Parser
import System.Environment ( getArgs )
import TCommon ( printListLn, format_ast )

main = do
    args <- getArgs
    let fn = head args
    srcs <- readFile fn
    let tokens = lexme srcs
    let ast = parse tokens 
    let fast = format_ast ast
    printListLn fast


