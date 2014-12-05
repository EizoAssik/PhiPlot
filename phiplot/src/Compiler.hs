module Compiler where

import Structure

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

compile_fn (Ref "draw") = ["DRAW"]
compile_fn (Ref fn) =     ["CALL", '@':fn]
compile_ref (Ref val) =   ["PUSH", '&':val]
compile_atom (Ref val) =  ["PUSH", '&':val, "LOAD"]
compile_atom (Imm n) =    ["PUSH", show n]
compile_atom (Subexpr expr) = compile_expr expr
-- Hopes that the parser will never send wrong sructures...
compile_atom (Funcall fn args) =
    (compile_expr args) ++ (compile_fn fn)
compile_atom (Negative expr) = (compile_expr expr) ++ ["NEG"]

compile_expr Skip = ["NOP"]
compile_expr (Elem atom) = compile_atom atom
compile_expr (BinOp tk l r) = 
    (compile_expr l) ++ (compile_expr r) ++ (dump_token tk)
compile_expr (Logic tk l r) = 
    (compile_expr l) ++ (compile_expr r) ++ (dump_token tk)
compile_expr (ArgList car cdr) = 
    (compile_expr car) ++ (compile_expr cdr)
compile_expr (PhiList car cdr) = 
    (compile_expr car) ++ (compile_expr cdr)

compile_stmt (ExprEval expr) = compile_expr expr
compile_stmt (Direct atom) = compile_atom atom
compile_stmt (Return expr) = (compile_expr expr) ++ ["RET"]
compile_stmt END = ["HALT"]
compile_stmt NOP = ["NOP"]
compile_stmt (Set ref expr) =
    (compile_ref ref) ++ (compile_expr expr) ++ ["STORE"]
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
    in  ("@"++name):binding ++ code


replace_local_var [] _ _ = []
replace_local_var (x:xs) params prefix = 
    let this = if (head x == '&') && (elem (tail x) params)
                  then "&"++prefix++"-"++(tail x)
                  else x
    in  this:(replace_local_var xs params prefix)

compile [] = []
compile ((Success def@(Def _ _ _) _):xs) = (compile xs) ++ (compile_stmt def)
compile ((Success stmt _):xs) = (compile_stmt stmt) ++ compile xs


