module Compiler where

import Structure

dump_token tk = [show tk]

arglist_length al = 
    case al of
        PhiList l r -> (arglist_length l) + (arglist_length r)
        _ -> 1

compile_fn (Ref "draw") =    ["DRAW"]
compile_fn (Ref fn) =    ["CALL", ':':fn]
compile_ref (Ref val) =  ["PUSH", '&':val]
compile_atom (Ref val) = ["PUSH", '&':val, "LOAD"]
compile_atom (Imm n) =   ["PUSH", show n]
compile_atom (Subexpr expr) = compile_expr expr
-- Hopes that the parser will never send wrong sructures...
compile_atom (Funcall fn args) =
    (compile_expr args) ++ (compile_fn fn)

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


compile (Success stmt _) = compile_stmt stmt


