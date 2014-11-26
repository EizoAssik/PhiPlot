module Main where

import Lexer
import System.Environment ( getArgs )

main = do
    args <- getArgs
    let fn = head args
    srcs <- readFile $ head args
    print $ lexme srcs
