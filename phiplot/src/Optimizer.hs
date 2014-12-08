module Optimizer where

import Debug.Trace

import Structure

opt_atom atom = 
    case atom of
        Not      expr -> Not $ opt_expr expr
        Subexpr  expr -> Subexpr $ opt_expr expr
        Negative expr -> Negative $ opt_expr expr
        Funcall ref expr -> Funcall ref $ opt_expr expr
        _     -> atom
-- 一个表达式是可优化的，当且仅当表达式匹配下列情形之一
--          Elem    Atom
--          PhiList Expr  Expr
--          ArgList Expr  Expr
--          BinOp   Token Expr Expr
--          Logic   Token Expr Expr
--          Skip

maybeFold (Elem atom) = 
    case atom of
        Imm _          -> True
        Not expr       -> maybeFold expr
        Subexpr  expr  -> maybeFold expr
        Negative expr  -> maybeFold expr
        Funcall _ expr -> maybeFold expr
        _              -> False
maybeFold (BinOp _ l r) = maybeFold l && maybeFold r
maybeFold _ = False

opt_expr (Elem atom) = 
    let e = Elem $ opt_atom atom
        _ = trace $ show e
    in e
opt_expr (BinOp tk (Elem (Imm l)) (Elem (Imm r))) = 
    let res = case tk of
                  ADD -> l + r
                  SUB -> l - r
                  MUL -> l * r
                  DIV -> l / r
    in  Elem(Imm res)
opt_expr (BinOp tk l r) = 
    let nl = if maybeFold l then opt_expr l else l
        nr = if maybeFold r then opt_expr r else r
        next = BinOp tk nl nr
        _ = trace $ show next
    in  if nl/=l && nr/=r  && maybeFold next
            then opt_expr next
            else next

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
