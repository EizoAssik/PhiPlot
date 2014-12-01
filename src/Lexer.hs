module Lexer (Token(..), lexme) where

import Data.Char

-- ERROR 用于在语法分析阶段标记错误
data Token = Name     String
           | Real     Float
           | LP  | RP   | LB  | RB   | COMMA | SEMICOLON
           | FOR | FROM | TO  | STEP | IS  | ERROR | EOF
           | ADD | SUB  | MUL |  DIV | POWER | Unknown String
           deriving (Show, Eq)

reserved = [
    ("=",    IS),
    ("is",   IS),
    ("for",  FOR),
    ("from", FROM),
    ("to",   TO),
    ("step", STEP),
    ("IS",   IS),
    ("FOR",  FOR),
    ("FROM", FROM),
    ("TO",   TO),
    ("STEP", STEP),
    ("(",    LP),
    (")",    RP),
    ("{",    LB),
    ("}",    RB),
    (",",    COMMA),
    ("+",    ADD),
    ("-",    SUB),
    ("*",    MUL),
    ("/",    DIV),
    (";",    SEMICOLON)]

lookupReserved :: String -> Maybe Token
lookupReserved name = lookup name reserved

detectSymbol :: Char -> Token
detectSymbol char = detectLiteral [char] Unknown

detectName :: String -> Token
detectName name = detectLiteral name Name

detectLiteral :: String -> (String -> Token) -> Token
detectLiteral name tkcons = case lookupReserved name of
                      Just tk -> tk
                      Nothing -> tkcons name 

readNumber :: String -> String -> (Token, String)
readNumber [] num = (Real (read.reverse$num :: Float), []) 
readNumber src@(curr:remains) num
    | (isDigit curr) || (curr == '.') = readNumber remains (curr : num)
    | otherwise = (Real (read.reverse$num :: Float), src)

readName :: String -> String -> (Token, String)
readName [] name =  (detectName.reverse$name, [])
readName src@(curr:remains) name
    | isAlpha curr = readName remains (curr : name)
    | (isDigit curr) && (not.null$name) = readName remains (curr : name)
    | otherwise = (detectName.reverse$name, src)

skipComment :: String -> String
skipComment [] = []
skipComment (curr:remains) = 
    if curr /= '\n'
        then skipComment remains
        else remains

lexme :: String -> [Token]
lexme [] = []
lexme ('/':'/':remains) = lexme.skipComment $ remains
lexme ('*':'*':remains) = POWER : lexme remains
lexme src@(curr:remains)
    | isSpace curr = lexme remains
    | isDigit curr = 
        let (num, remains) = readNumber src ""
        in  num : lexme remains
    | isAlpha curr =
        let (name, remains) = readName src ""
        in  name : (lexme remains)
    | otherwise    = (detectSymbol curr) : lexme remains 
