module Language.PhiPlot.Lexer where

import Data.Char

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tk

ops  = ["+", "*", "-", "/", ";", "<", ">", "<=", ">=", "==", "!="]

names = [ "def"
        , "extern"
        , "return"
        , "if"
        , "else"
        , "while" ]

lexer :: Tk.TokenParser ()
lexer = Tk.makeTokenParser $ emptyDef
  { Tk.commentLine = "//"
  , Tk.reservedOpNames = ops
  , Tk.reservedNames = names
  , Tk.caseSensitive = False }

float :: Parser Double
float = Tk.float lexer

intfloat :: Parser Double
intfloat = fmap fromIntegral $ Tk.integer lexer

parens :: Parser a -> Parser a
parens = Tk.parens lexer

braces :: Parser a -> Parser a
braces = Tk.braces lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tk.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tk.semiSep lexer

-- PhiPlot is NOT case sensitive
identifier :: Parser String
identifier = map toLower `fmap` Tk.identifier lexer

reserved :: String -> Parser ()
reserved = Tk.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tk.reservedOp lexer
