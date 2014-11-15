module Lexer (Token, lexme) where

import Data.Char

data Token = Name     String
           | Real     Float
           | Operator String 
           | Rest     String
           | LP  | RP   | LB  | RB   | COMMA | SEMICOLON
           | FOR | FROM | TO  | STEP | IS  | ERROR
           | ADD | SUB  | MUL |  DIV | POWER
           deriving (Show, Eq)

iRESERVED = [
    (IS,     "is"),
    (TO,     "to"),
    (FOR,    "for"),
    (FROM,   "from"),
    (STEP,   "step"),
    (LP,     "("),
    (RP,     ")"),
    (LB,     "{"),
    (RB,     "}"),
    (COMMA,  ","),
    (ADD,    "+"),
    (SUB,    "-"),
    (MUL,    "*"),
    (DIV,    "/"),
    (SEMICOLON, ";")] :: [(Token, String)]
 
searchTable :: String -> [(Token, String)] -> Token
searchTable _    []      = ERROR     
searchTable name (x:xs)  = if name == snd x 
                               then fst x
                               else searchTable name xs

detectName :: String -> Token
detectName name = let rev = searchTable name iRESERVED
                  in if rev == ERROR
                         then Name name
                         else rev

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
        in  name : lexme remains
    | otherwise    = (searchTable [curr] iRESERVED) : lexme remains 
