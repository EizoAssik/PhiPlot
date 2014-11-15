module Main where

import Lexer

main = do
    src <- getLine
    print $ lexme src
