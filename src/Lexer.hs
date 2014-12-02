module Lexer (Token(..), lexme) where

import Structure
import Data.Char hiding ( format )

reserved = [
    ("=",    IS),
    ("is",   IS),
    ("for",  FOR),
    ("from", FROM),
    ("to",   TO),
    ("step", STEP),
    ("if",   IF),
    ("else", ELSE),
    ("elif", ELIF),
    ("then", THEN),
    ("(",    LP),
    (")",    RP),
    ("{",    LB),
    ("}",    RB),
    (",",    COMMA),
    ("+",    ADD),
    ("-",    SUB),
    ("*",    MUL),
    ("/",    DIV),
    (";",    SEMICOLON),
    ("<",    Structure.LT),
    (">",    Structure.GT),
    ("==",   Structure.EQ),
    ("!=",   Structure.NE),
    ("<=",   Structure.LE),
    (">=",   Structure.GE),
-- 保留以兼容旧语法
    ("IS",   IS),
    ("FOR",  FOR),
    ("FROM", FROM),
    ("TO",   TO),
    ("STEP", STEP)]

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
