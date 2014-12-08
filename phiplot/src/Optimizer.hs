module Optimizer where

import Structure

opt_atom atom = 
    case atom of
        Not      expr -> Not $ opt_expr expr
        Subexpr  expr -> Subexpr $ opt_expr expr
        Negative expr -> Negative $ opt_expr expr
        _     -> atom

maybeFold (Elem atom) = 
    case atom of
        Imm _      -> True
        Not _      -> True
        Subexpr  _ -> True
        Negative _ -> True
        _          -> False
maybeFold (BinOp _ _ _) = True
maybeFold _ = False

opt_expr (BinOp tk (Elem (Imm l)) (Elem (Imm r))) = 
    let res = case tk of
                  ADD -> l + r
                  SUB -> l - r
                  MUL -> l * r
                  DIV -> l / r
    in  Elem(Imm res)

opt_expr this@(BinOp tk l r) = 
    BinOp tk (opt_expr l) (opt_expr r)

opt_expr e = e

opt_stmt stmt = 
    case stmt of
        ExprEval expr -> ExprEval $ opt_expr expr
        _             -> stmt

opt_ast [] = []
opt_ast (x:xs) =
    case x of
        Success r rs -> (Success (opt_stmt r) rs):(opt_ast xs)
        _            -> x:(opt_ast xs)
