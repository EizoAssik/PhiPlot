module Language.PhiPlot.Parser where

import Prelude hiding (Ordering(..))

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Language.PhiPlot.Lexer
import Language.PhiPlot.AST

-- Parse algebra expressions

binopl (s, f) = Ex.Infix (reservedOp s >> return (BinOp f)) Ex.AssocLeft

prefix (s, f) = Ex.Prefix (reservedOp s >> return (UniOp f))

algops = [ prefixs [ ("+", Positive), ("-", Negative) ] 
         , infixls [ ("*", Mul), ("/", Div) ]
         , infixls [ ("+", Plus), ("-", Minus) ] ]
         where infixls = map binopl
               prefixs = map prefix

aexpr :: Parser Expr
aexpr = Ex.buildExpressionParser algops afactor

afactor :: Parser Expr
afactor = try number
  <|> try call
  <|> variable
  <|> parens aexpr

-- Parser logic expressions

cmp (s, f) = Ex.Infix (reservedOp s >> return (Cmp f)) Ex.AssocLeft
bop (s, f) = Ex.Infix (reservedOp s >> return (LogicOp f)) Ex.AssocLeft

logops = [ [ Ex.Prefix (reservedOp "!" >> return Not) ] 
         , map cmp [ ("<", LT), (">", GT), ("<=", LE)
                   , (">=", GE), ("==", EQ), ("!=", NE) ]
         , map bop [ ("&&", AND), ("||", OR) ] ]

bexpr :: Parser BoolExpr
bexpr = Ex.buildExpressionParser logops bfactor

bfactor :: Parser BoolExpr
bfactor = try beatom <|> try nonzero <|> parens bexpr

beatom :: Parser BoolExpr
beatom = aexpr >>= return . BEAtrom

-- Componentions

number :: Parser Expr
number = try numberf <|> try numberi
  where numberf = float >>= return . Number
        numberi = intfloat >>= return . Number

variable :: Parser Expr
variable = identifier >>= return . Var

nonzero :: Parser BoolExpr
nonzero = (try number <|> try variable) >>= return . Nonzero
 
call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep aexpr
  return $ Call name args

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

-- Statements
stmt :: Parser AST
stmt = do
  v <-  try cond
    <|> try for
    <|> try while
    <|> try assign
    <|> try bstmt
    <|> try astmt
  optional $ reservedOp ";"
  return v

block :: Parser [AST]
block = many stmt

astmt :: Parser AST
astmt = aexpr >>= return . AExp

bstmt :: Parser AST
bstmt = bexpr >>= return . BExp

assign :: Parser AST
assign = do
  k <- identifier
  reservedOp "="
  v <- aexpr
  return $ Assign k v

defun :: Parser AST
defun = do
  reserved "def"
  name <- identifier
  args <- parens $ commaSep variable
  body <- braces $ many stmt
  return $ Def name args body

cond = do
  reserved "if"
  c <- parens $ bexpr
  t <- block
  reserved "else"
  f <- block
  return $ If c t f

for = do
  reserved "for"
  v <- identifier
  reserved "from"
  s <- aexpr
  reserved "to"
  e <- aexpr
  reserved "step"
  d <- aexpr
  b <- many stmt
  return $ For v s e d b

while = do
  reserved "while"
  c <- bexpr
  b <- many stmt
  return $ While c b

-- The full parser

toplevel :: Parser [AST]
toplevel = many $ try stmt <|> try defun

parseAExpr :: String -> Either ParseError Expr
parseAExpr s = parse (contents aexpr) "<stdin>" s

parseBExpr :: String -> Either ParseError BoolExpr
parseBExpr s = parse (contents bexpr) "<stdin>" s

parseToplevel :: String -> Either ParseError [AST]
parseToplevel s = parse (contents toplevel) "<stdin>" s

phiParser = parseToplevel

