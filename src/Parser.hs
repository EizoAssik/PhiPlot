module Parser ( parse, ParseResult(..) ) where

import Structure

parse_atom (Real r:rs) = Success (Imm r) rs
parse_atom (Name n:[]) = Success (Ref n) []
parse_atom (Name n:LP:rs) = case parse_list rs of
                  Success pl (RP:xs) -> Success (Funcall (Ref n) pl) xs
                  _ -> Error LP (tail rs) "RP lost in Funcall"
parse_atom (Name n:rs) = Success (Ref n) rs
parse_atom (SUB:rs) = 
    case parse_atom rs of
        Success atom rs -> Success (Negative $ Elem atom) rs
        Error tk rs er -> Error tk rs ("Error in parsing Negative Atom: " ++ er)
parse_atom (NOT:rs) = 
    case parse_expr rs of
        Success expr rs -> Success (Not expr) rs
        Error tk rs er -> Error tk rs ("Error in parsing NOT logic: " ++ er)
parse_atom tks@(LP:rs) =
    case parse_list rs of
        succ@(Success expr (RP:xs)) ->
            Success (Subexpr expr) xs
        err@(Error c s info) -> Error c s info
parse_atom (tk:rs) = Error tk rs ("Token: " ++ show tk ++ " is not an atom")
parse_atom [] = Success Void []


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
-- 同优先级双目操作符表达式串的通用递归分析器
-- 目前用于解析term序列、expr序列和PhiList
-- constructor Expr的构造子
-- functor 扫描函数
-- test 操作符判定
brp constructor functor test tks = 
    case functor tks of
        succ@(Success tl []) -> succ
        succ@(Success tl xs@(x:s)) ->
            if test xs
                then brp_inner constructor functor test [(succ, x)] s
                else succ
        err@(Error _ _ _) -> err

brp_inner constructor functor test stack []  =
    brp_constructor constructor $ reverse stack
brp_inner constructor functor test stack tks =
    if (snd (head stack)) == EOF
        then brp_constructor constructor $ reverse stack 
        else case functor tks of
                 succ@(Success expr []) ->
                     brp_inner constructor functor test ((succ, EOF):stack) []
                 succ@(Success expr xs@(x:s)) ->
                     if test xs
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

parse_term tks = brp (\op l r -> BinOp op l r) parse_factor (\(x:s)->x==MUL || x==DIV) tks
parse_expr tks = brp (\op l r -> BinOp op l r) parse_term (\(x:s)->x==ADD || x==SUB) tks
parse_list tks = brp (\op l r -> PhiList l r) parse_expr (\(x:s)->x==COMMA) tks
parse_cmp  tks = brp (\op l r -> BinOp op l r) parse_term (\(x:s)->x==ADD || x==SUB) tks

parse_stmts tks =
    (brp
     (\op l r -> Block l r)
     parse_stmt
     (\x -> case x of
                RB:_ -> False
                SEMICOLON:RB:_ -> False
                SEMICOLON:_ -> True
                _ -> False)
     tks) |=?>> SEMICOLON |=?>> RB

inner_make_for parse_next last rs = 
    case parse_next rs of
        err@(Error x s i) -> Error x s i
        Success x xs -> case last of
            For atom Skip Skip Skip NOP ->
                Success (For atom x Skip Skip NOP) xs
            For atom from Skip Skip NOP ->
                Success (For atom from x Skip NOP) xs
            For atom from to   Skip NOP ->
                Success (For atom from to x   NOP) xs

parse_for tks =
    let feed_expr = (inner_make_for parse_expr)
        atom = parse_atom tks
        base = case atom of
                   Error x s i  -> Error x s i
                   Success a@(Ref r) rs -> Success (For a Skip Skip Skip NOP) rs
        iter = base |=?> FROM |-> feed_expr |=?> TO |-> feed_expr
                    |=?> STEP |-> feed_expr
        Success (For a f t s _) (n:ns) = iter
    in if n == LB
        then parse_stmts ns |-> (\block xs -> Success (For a f t s block) xs)
        else parse_expr  ns |-> (\expr  xs -> Success (For a f t s (ExprEval expr)) xs)

    
inner_make_set parse_next last rs = 
    case parse_next rs of
        err@(Error x s i) -> Error x s i
        Success x xs ->
            let Set a _ = last
            in  Success (Set a x) xs

parse_set tks =
    let feed_expr = (inner_make_set parse_expr)
        atom = parse_atom tks
        base = case atom of
                   Error x s i  -> Error x s i
                   Success a rs -> Success (Set a Skip) rs
    in base |=?> IS |-> feed_expr                                  

parse_direct tks = parse_atom tks |-> (\p rs -> Success (Direct p) rs)
parse_eval tks = parse_expr tks |-> (\p rs -> Success (ExprEval p) rs)

parse_stmt [] = Success END []
parse_stmt (SEMICOLON:rs) = parse_stmt rs
parse_stmt (LB:rs) = parse_stmts rs
parse_stmt (FOR:rs) = parse_for rs
parse_stmt tks@(Name _:LP:rs)        = parse_direct tks
parse_stmt tks@(Name _:SEMICOLON:rs) = parse_direct tks
parse_stmt tks@(Name _:[]) = parse_direct tks
parse_stmt tks@(Name _:rs) = parse_set tks
parse_stmt tks = parse_eval tks

inner_parse [] [] = []
inner_parse [] st = reverse st
inner_parse tk stmts = 
    case parse_stmt tk of
        succ@(Success stmt xs) -> inner_parse xs (succ:stmts)
        Error etk r err -> [Error etk r err]

parse tk = inner_parse tk []
