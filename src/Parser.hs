module Parser where

import Lexer ( Token(..) )

data Atom = Imm Float
          | Ref String
          | Subexpr Expr
          | Negative Atom
          deriving (Show, Eq)

data Expr = Elem    Atom
          | BinOp   Token  Expr  Expr
          | Funcall Atom  [Expr]
          deriving (Show, Eq)

data Stmt = Set Atom Expr
          | For Atom Expr Expr Expr [Stmt]
          | END | EOF | ERROR
          | Expr -- Just for test
          deriving (Show, Eq)

data ParseResult r = Success r [Token]
                   | Error   Token [Token] String
                   deriving (Show, Eq)

parse_atom (Real r:tks) = Success (Imm r) tks
parse_atom (Name n:tks) = Success (Ref n) tks
parse_atom (SUB:tks) = 
    case parse_atom tks of
        Success atom rs -> Success (Negative atom) rs
        Error tk tks er -> Error tk tks ("Error in parsing Negative Atom: " ++ er)
parse_atom (tk:tks) = Error tk tks "Not a atom" 

parse_args tks@(LP:rs) = Success [] tks

-- parse_expr tks =
--     case parse_atom tks of
--         Success rf@(Ref _) rs ->
--             case parse_args rs of
--                 Success args ars -> Success (Funcall rf args) ars
--                 Error ca ars err -> Error ca ars ("Error in parsing Funcall" ++ err)
--         Success se@(Subexpr _) rs ->
--             case head rs of -> 
--
parse_for rs = Success (For (Imm 42)(Elem $ Imm 42)(Elem $ Imm 42)(Elem $ Imm 42) []) rs
                
parse_set rs = Success (Set (Imm 42) (Elem $ Imm 42)) rs

parse_stmt tks@(tk:rs) = 
    case tk of 
        FOR    -> parse_for rs
        Name n -> parse_set rs
        _      -> Error tk rs "Unknown statment"

inner_parse [] [] = Success [END] []
inner_parse [] st = Success (reverse $ END:st) []
inner_parse tks@(tk:rs) stmts = 
    case parse_stmt tks of
        Success stmt xs -> inner_parse xs (stmt:stmts)
        Error etk r err -> Error etk r err

parse tk = inner_parse tk []
