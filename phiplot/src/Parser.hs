module Parser ( parse, ParseResult(..), build_ast_stmt ) where

import Structure as ST

parse_atom (Real r:rs) = Success (Imm r) rs
parse_atom (Name "PI":rs) = Success (Imm $ pi) rs
parse_atom (Name "E":rs) = Success (Imm $ exp 1) rs
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
        err@(Error _ _ _) -> wrap err "When parsing sub-expr with `()'"
parse_atom (tk:rs) = Error tk rs ("Token " ++ show tk ++ " is not an atom.")
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
        err@(Error _ _ _) -> wrap err "When using BRP"

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
brp_constructor constructor ((err@(Error _ _ _), EOF):s)  = err
brp_constructor constructor (_:(err@(Error _ _ _), EOF):_) = err
brp_constructor constructor ((lr, lop):(rr, rop):s) = 
    let Success litem _ = lr
        Success ritem r = rr
        next = ((Success (constructor lop litem ritem) r), rop):s
    in  brp_constructor constructor next
-- BRP 结束                                       

tk_is_cmp (ST.EQ:_) = True 
tk_is_cmp (ST.NE:_) = True 
tk_is_cmp (ST.LT:_) = True 
tk_is_cmp (ST.GT:_) = True 
tk_is_cmp (ST.LE:_) = True 
tk_is_cmp (ST.GE:_) = True 
tk_is_cmp _ = False

build_binop op l r = BinOp op l r
parse_term tks = brp build_binop parse_factor (\(x:s)->x==MUL || x==DIV) tks
parse_expr tks = brp build_binop parse_term   (\(x:s)->x==ADD || x==SUB) tks
parse_cmp  tks = brp build_binop parse_expr   tk_is_cmp tks
parse_and  tks = brp build_binop parse_cmp    (\(x:s)->x==AND) tks
parse_logic tks = 
    brp (\op l r -> Logic op l r) parse_and (\(x:s)->x==OR) tks

parse_list xs@(RP:rs) = Success (PhiList Skip Skip) xs
parse_list tks = brp (\op l r -> PhiList l r)  parse_expr (\(x:s)->x==COMMA) tks

parse_args xs@(RP:rs) = Success (ArgList Skip Skip) rs
parse_args tks = brp (\op l r -> ArgList l r)  parse_expr (\(x:s)->x==COMMA) tks
                 |=?> RP

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
                   Success a rs -> Success (Set a Skip) rs
                   err          -> wrap err "When constructing set stmt's left-value"
    in base |=?> IS |-> feed_expr                                  

if_feed_block last (LB:xs) =
    case parse_stmts xs of
        Success block rs ->
            case last of
                If c NOP NOP -> Success (If c block NOP) rs
                If c t   NOP -> Success (If c t   block) rs
        err -> wrap err "When parsing block for IF stmt's body"

if_feed_block last (ELIF:xs) =
    case parse_if xs of
        Success stmt rs ->
            case last of
                If c NOP NOP -> Success (If c stmt  NOP) rs
                If c t   NOP -> Success (If c t    stmt) rs
        err -> wrap err "When parsing stmt for IF stmt's else-case"

if_feed_block last (ELSE:xs) = if_feed_block last xs

if_feed_block last xs =
    case parse_stmt xs of
        Success stmt rs ->
            case last of
                If c NOP NOP -> Success (If c stmt NOP) rs
                If c t   NOP -> Success (If c t   stmt) rs
        err -> wrap err "When parsing stmt for IF stmt's case"

parse_if tks = 
    let condition = case parse_logic tks |=?> THEN of
                        Success c rs -> Success (If c NOP NOP) rs
                        err          -> wrap err "When parsing condition for IF stmt"
    in  condition |-> if_feed_block |-> if_feed_block

inner_make_def_body (Def fn@(Ref name) args NOP) rs =
    case parse_stmts rs of
        Success v xs -> Success (Def fn args v) xs
        err -> wrap err $ "When parsing the definition of function "++name++"'s body"

inner_make_def_args (Def fn@(Ref name) Skip NOP) rs =
    case parse_args rs of
        Success v xs -> Success (Def fn v NOP) xs
        err -> wrap err $ "When parsing the definition of function "++name++"'s args"

parse_def (Name fn:rs) =
    (Success (Def (Ref fn) Skip NOP) rs) |=?> LP 
                                         |->  inner_make_def_args
                                         |=?> LB
                                         |->  inner_make_def_body

parse_def (x:s) = 
    Error x s (show x ++ " is not a name")

parse_direct tks = parse_atom tks |-> (\p rs -> Success (Direct p) rs)
parse_eval tks = parse_expr tks |-> (\p rs -> Success (ExprEval p) rs)

parse_stmt (LB:rs) = parse_stmts rs
parse_stmt (IF:rs) = parse_if rs
parse_stmt (FOR:rs) = parse_for rs
parse_stmt (DEF:rs) = parse_def rs
parse_stmt tks@(Name _:LP:rs)        = parse_direct tks
parse_stmt tks@(Name _:SEMICOLON:rs) = parse_direct tks
parse_stmt tks@(Name _:[]) = parse_direct tks
parse_stmt tks@(Name _:IS:rs) = parse_set tks
parse_stmt tks@(RETURN:rs) = parse_expr rs |-> (\x s -> Success (Return x) s)
parse_stmt (SEMICOLON:rs) = parse_stmt rs
parse_stmt tks = parse_eval tks

inner_parse [] [] = []
inner_parse [] st = reverse $ (Success END []):st
inner_parse tk stmts = 
    case parse_stmt tk of
        succ@(Success stmt xs) -> inner_parse xs (succ:stmts)
        Error etk r err -> [Error etk r err]

parse tk = inner_parse tk []

-- 从Expr的层面构建AST
build_ast_atom (Ref ref) = REF ref
build_ast_atom (Imm num) = NUM num
build_ast_atom (Funcall (Ref fn) args) =
    case args of 
        PhiList l r -> CALL fn $ build_ast ~>> args
        _ -> CALL fn $ build_ast ~>> args
build_ast_atom (Subexpr expr) = build_ast expr
build_ast_atom (Negative expr) = SOP SUB $ build_ast expr
build_ast_atom (Not expr) = SOP NOT $ build_ast expr

-- 构建其他嵌套式结构
build_ast (Elem atom) = build_ast_atom atom
build_ast (BinOp tk l r) = OP tk (build_ast l) (build_ast r)
build_ast (Logic tk l r) = OP tk (build_ast l) (build_ast r)
build_ast Skip = PASS


build_ast_stmt (Direct atom)        = build_ast_atom atom
build_ast_stmt (ExprEval expr)      = build_ast expr
build_ast_stmt (Return  expr)       = SOP RETURN $ build_ast expr
build_ast_stmt block@(Block _ _)    = BLOCK $ build_ast_stmt ~>> block
build_ast_stmt (Set target expr)    =
    OP IS (build_ast_atom target) (build_ast expr)
build_ast_stmt (If cond e1 e2)      = COND (build_ast cond)
                                           (build_ast_stmt e1)
                                           (build_ast_stmt e2)
build_ast_stmt (For (Ref val) from to step body) = 
    LOOP val
         (build_ast from)
         (build_ast to)
         (build_ast step)
         (build_ast_stmt body)
build_ast_stmt (Def fn args body) = 
    DEFINE (name fn) ((name . ST.elem) ~>> args) (build_ast_stmt body)
