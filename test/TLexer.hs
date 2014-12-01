module Main where

import Lexer ( lexme, Token(..) )
import Data.Char ( toLower )
import System.Environment ( getArgs )

format_token (Name n:rs) ss = format_token rs (("Name:  " ++ n):ss)
format_token (Real r:rs) ss = format_token rs (("Real:  " ++ (show r)):ss)
format_token (tk:rs)     ss = format_token rs (("Token: " ++ (show tk)):ss)
format_token []          ss = ss

printListLn [] = putStrLn "Done."
printListLn (ss:rs) = do
    putStrLn ss
    printListLn rs

main = do
    args <- getArgs
    let fn = head args
    srcs <- readFile $ head args
    let tokens = lexme srcs
        tkstr  = reverse $ format_token tokens []
    printListLn tkstr

