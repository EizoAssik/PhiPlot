module Main where

import Lexer
import Parser
import Compiler
import Preprocessor ( alloc, trans_jmp )
import System.Environment ( getArgs )
import TCommon ( printListLn, format_ast )

main = do
    args <- getArgs
    let fn = head args
    srcs <- readFile fn
    let tokens = lexme srcs
    let ast = parse tokens 
    let fast = trans_jmp $ alloc $ foldl1 (++) $ map compile ast
    printListLn fast


