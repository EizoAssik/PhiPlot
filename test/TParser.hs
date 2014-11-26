
module Main where

import Lexer
import System.Environment ( getArgs )

main = do
    args <- getArgs
    let fn = head args
    srcs <- readFile fn
    let tokens = lexme srcs
    let ast = parse tokens 
    print ast


