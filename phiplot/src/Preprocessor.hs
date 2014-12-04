-- 预处理器仅仅面向字符串处理

module Preprocessor where

import Data.List ( (\\) )

inner_alloc _ [] table = table
inner_alloc vc (x:xs) table = 
    if head x == '&'
        then case lookup x table of
                 Nothing -> inner_alloc (vc + 1) xs ((x, show vc):table)
                 Just _  -> inner_alloc vc xs table
        else inner_alloc (vc + 1) xs table

inner_replace [] _ = []
inner_replace (x:xs) table =
    case lookup x table of
        Just pc -> pc:(inner_replace xs table)
        Nothing ->  x:(inner_replace xs table)

alloc codes = 
    let address = inner_alloc 0 codes []
    in  inner_replace codes address
        
inner_jmp _ [] = []
inner_jmp pc (x:xs) = 
    let rest = inner_jmp (pc+1) xs
    in  if ":+" == take 2 x
            then (show $ pc + (read (x \\ ":+") :: Int)):rest
            else x:rest

trans_jmp code = inner_jmp 0 code
