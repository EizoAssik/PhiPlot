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

trans_fcall codes = 
    let address = inner_locate_fcall 0 codes []
        codes_cd = comment_def_label 0 codes address
    in  inner_replace_funcall codes_cd address
    
inner_locate_fcall _ [] table = table
inner_locate_fcall pc (x:xs) table = 
    let new_table = if '@' == head x
        then case lookup x table of
                 Nothing -> (x, pc):table
                 Just n  -> update table x pc
        else table
    in  inner_locate_fcall (pc+1) xs new_table

inner_replace_funcall [] _ = []
inner_replace_funcall (x:xs) address =
    let rest = inner_replace_funcall xs address
    in case lookup x address of
           Nothing -> x:rest
           Just n  -> (show n):rest

update [] _ _ = []     
update (t@(k,v):xs) key value = 
    let rest = update xs key value
    in if k == key
           then (key, value):rest
           else t:rest

comment_def_label _ xs [] = xs
comment_def_label _ [] _  = []
comment_def_label pc (x:xs) address =
    let  rest = comment_def_label (pc+1) xs address
    in  case lookup x address of
            Nothing -> x:rest
            Just n  -> if n == pc
                           then rest
                           else x:rest
