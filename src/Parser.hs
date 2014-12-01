module Parser where

import Lexer ( Token(..) )

data Atom = Imm Float
          | Ref String
          | Subexpr Expr
          | Negative Expr
          | Funcall Atom Expr
          | Void
          deriving (Show, Eq)

data Expr = Elem    Atom
          | BinOp   Token  Expr  Expr
          | PhiList Expr Expr
          deriving (Show, Eq)

data Stmt = Set Atom Expr
          | For Atom Expr Expr Expr [Stmt]
          | END | ERROR
          deriving (Show, Eq)

data ParseResult r = Success r [Token]
                   | Error   Token [Token] String
                   deriving (Show, Eq)

parse_atom (Real r:rs) = Success (Imm r) rs
parse_atom (Name n:rs) = 
    case head rs of
        LP -> case parse_list $ tail rs of
                  Success pl (RP:xs) -> Success (Funcall (Ref n) pl) xs
                  _ -> Error LP (tail rs) "RP lost in Funcall"
        _  -> Success (Ref n) rs
parse_atom (SUB:rs) = 
    case parse_atom rs of
        Success atom rs -> Success (Negative $ Elem atom) rs
        Error tk rs er -> Error tk rs ("Error in parsing Negative Atom: " ++ er)
parse_atom tks@(LP:rs) =
    case parse_expr rs of
        succ@(Success expr (RP:xs)) ->
            Success (Subexpr expr) xs
        err@(Error c s info) -> Error c s info
parse_atom (tk:rs) = Error tk rs "Not a atom" 


parse_power :: [Token] -> ParseResult Expr
parse_power tks =
    case parse_atom tks of
        succ@(Success base []) -> Success (Elem base) []
        succ@(Success base rs) ->
            case head rs of
                POWER -> case parse_power $ tail rs of
                             Success exp xs ->
                                 Success (BinOp POWER (Elem base) exp) xs
                             err@(Error x xs info) -> Error x xs info 
                _     -> Success (Elem base) rs
        err@(Error x xs info) -> Error x xs info 


parse_factor (ADD:rs) = parse_factor rs
parse_factor (SUB:rs) = 
    case parse_factor rs of
        Success factor rs -> Success (Elem $ Negative factor) rs
        Error tk rs er -> Error tk rs ("Error in parsing negative factor: " ++ er)
parse_factor tks = parse_power tks

-- BinOP recursive parser
-- 双操作数表达式串的通用递归分析器
-- 目前用于解析term序列、expr序列和PhiList
-- constructor Expr的构造子
-- functor 扫描函数
-- test 操作符判定
brp constructor functor test tks = 
    case functor tks of
        succ@(Success tl []) -> succ
        succ@(Success tl xs@(x:s)) ->
            if test x
                then brp_inner constructor functor test [(succ, x)] s
                else succ

brp_inner constructor functor test stack []  =
    brp_constructor constructor $ reverse stack
brp_inner constructor functor test stack tks =
    if (snd (head stack)) == EOF
        then brp_constructor constructor $ reverse stack 
        else case functor tks of
                 succ@(Success expr []) ->
                     brp_inner constructor functor test ((succ, EOF):stack) []
                 succ@(Success expr xs@(x:s)) ->
                     if test x
                         then brp_inner constructor functor test ((succ, x):stack)   s
                         else brp_inner constructor functor test ((succ, EOF):stack) xs
                 err@(Error _ rs _) ->
                     brp_inner constructor functor test ((err, EOF):stack) rs
                  
brp_constructor constructor ((r,   EOF):[]) = r
brp_constructor constructor ((err, EOF):s)  = err
brp_constructor constructor ((lr, lop):(rr, rop):s) = 
    let Success litem _ = lr
        Success ritem r = rr
        next = ((Success (constructor lop litem ritem) r), rop):s
    in  brp_constructor constructor next
-- BRP 结束                                       

parse_term tks = brp (\op l r -> BinOp op l r) parse_factor (\x-> x==MUL || x==DIV) tks
parse_expr tks = brp (\op l r -> BinOp op l r) parse_term (\x-> x==ADD || x==SUB) tks
parse_list tks = brp (\op l r -> PhiList l r) parse_expr (\x -> x==COMMA) tks

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
