module Optimizer where

import Structure

opt_binop (BinOp tk (Elem (Imm l)) (Elem (Imm r))) = 
    let res = case tk of
                  ADD -> l + r
                  SUB -> l - r
                  MUL -> l * r
                  DIV -> l / r
    in  Elem(Imm res)
opt_binop (BinOp tk l r) = 
    BinOp tk (opt_binop l) (opt_binop r)
opt_binop e = e

