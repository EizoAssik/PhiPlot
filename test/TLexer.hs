module Main where

import Lexer ( lexme )
import Data.Char ( toLower )
import System.Environment ( getArgs )

main = do
    args <- getArgs
    let fn = head args
    srcs <- readFile $ head args
    print $ lexme srcs
