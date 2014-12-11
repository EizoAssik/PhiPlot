module Compiler where

import Structure as ST

dump_token tk = [show tk]

list_length al = 
    case al of
        PhiList l r -> (list_length l) + (list_length r)
        ArgList l r -> (list_length l) + (list_length r)
        Skip -> 0
        _ -> 1

list_expand ls = 
    case ls of
        ArgList l r -> (list_expand l) ++ (list_expand r)
        PhiList l r -> (list_expand l) ++ (list_expand r)
        Skip -> []
        expr -> [expr]

param_expand pl = map (\x -> let Elem (Ref ref) = x in ref) $ list_expand pl

builtins = [
        ("draw",   "DRAW"),
        ("sin",    "SIN"),
        ("cos",    "COS"),
        ("tan",    "TAN"),
        ("ROT",    "XROT"),
        ("SCALE",  "XSCL"),
        ("COLOR",  "XCLR"),
        ("ORIGIN", "XORG")]
compile_fn (Ref fn) = 
     case lookup fn builtins of 
         Just fn -> [fn]
         Nothing -> ["CALL", '@':fn]

compile_ref (Ref val)   = ["PUSH", '&':val]
compile_atom (Not expr) = (compile_expr expr) ++ ["NOT"]
compile_atom (Ref val)  = ["PUSH", '&':val, "LOAD"]
compile_atom (Imm n)    = ["PUSH", show n]
compile_atom (Subexpr expr) = compile_expr expr
-- Hopes that the parser will never send wrong sructures...
compile_atom (Funcall fn args) =
    (compile_expr args) ++ (compile_fn fn)
compile_atom (Negative expr) = (compile_expr expr) ++ ["NEG"]
compile_atom Void = []
compile_expr Skip = ["NOP"]
compile_expr (Elem atom) = compile_atom atom
compile_expr (BinOp tk l r) = 
    (compile_expr l) ++ (compile_expr r) ++ (dump_token tk)
compile_expr (ArgList car cdr) = 
    (compile_expr car) ++ (compile_expr cdr)
compile_expr (PhiList car cdr) = 
    (compile_expr car) ++ (compile_expr cdr)
compile_expr (Logic tk l r) = 
    let opcode = case tk of
                     Structure.LT -> ["LT"]
                     Structure.LE -> ["GT", "NOT"]
                     Structure.EQ -> ["EQ"]
                     Structure.GE -> ["LT", "NOT"]
                     Structure.GT -> ["GT"]
                     Structure.NE -> ["EQ", "NOT"]
    in  (compile_expr l) ++ (compile_expr r) ++ opcode

compile_stmt (ExprEval expr) = compile_expr expr
compile_stmt (Direct atom) = compile_atom atom
compile_stmt (Return expr) = (compile_expr expr) ++ ["RET"]
compile_stmt END = ["HALT"]
compile_stmt NOP = ["NOP"]
compile_stmt (Set ref@(Ref fn) expr) =
    case lookup fn builtins of
        Just x  -> (compile_expr expr) ++ [x]
        Nothing -> (compile_ref ref) ++ (compile_expr expr) ++ ["STORE"]
compile_stmt (Block l r) =
    (compile_stmt l) ++ (compile_stmt r)
compile_stmt (If cond succ fail) =
    let cond_code = compile_expr cond
        succ_code = compile_stmt succ
        fail_code = compile_stmt fail
    in  cond_code ++ ["JZ", ":+"  ++ (show $ (3+) $ length succ_code)]
                  ++ succ_code
                  ++ ["JMP", ":+" ++ (show $ (1+) $ length fail_code)]
                  ++ fail_code
compile_stmt (For ref from to step stmts) = 
    let to_code   = compile_expr to
        ref_code  = compile_ref  ref
        from_code = compile_expr from
        step_code = compile_expr step
        body_code = compile_stmt stmts
        jmp_off1  = 6 + 2 * (length ref_code)
                      + length step_code
                      + length body_code
        jmp_off2  = jmp_off1 + 2 + length ref_code + length to_code 
    in ref_code ++ from_code
                ++ ["STORE"]
                ++ ref_code
                ++ ["LOAD"]
                ++ to_code
                ++ ["GT", "JP", ":+" ++ show jmp_off1]
                ++ body_code
                ++ ref_code
                ++ ref_code ++ ["LOAD"]
                ++ step_code
                ++ ["ADD", "STORE"]
                ++ ["JMP", ":-" ++ show jmp_off2]
    

compile_stmt (Def fn args body) = 
    let Ref name = fn
        params = param_expand args
        param_count = length params
        binding = foldl1 (++) $ map (\x-> ["PUSH",
                                           "&"++name++"-"++x,
                                           "SWAP",
                                           "STORE"])
                                    (reverse params)
        code_raw = compile_stmt body
        code = replace_local_var code_raw params name
    in  ("@@"++name):binding ++ code


replace_local_var [] _ _ = []
replace_local_var (x:xs) params prefix = 
    let this = if (head x == '&') && (elem (tail x) params)
                  then "&"++prefix++"-"++(tail x)
                  else x
    in  this:(replace_local_var xs params prefix)

compile [] = []
compile ((Success def@(Def _ _ _) _):xs) = (compile xs) ++ (compile_stmt def)
compile ((Success stmt _):xs) = (compile_stmt stmt) ++ compile xs

sym <~~ table = 
    case lookup sym table of
        Nothing -> let itemname = "t"++(show $ length table)
                   in  (REF itemname, (sym, itemname):table)
        Just n  -> (REF n, table)

trituple (OP tk l r) table = 
    let (ll, tl) = trituple l table
        (rr, tr) = trituple r tl
    in  (OP tk ll rr) <~~ tr
trituple ast table = ast <~~ table    
