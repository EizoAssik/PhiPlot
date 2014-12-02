module TCommon ( printListLn, format_token, format_ast ) where

import Structure

inner_format_token a = 
    case a of 
        Name n -> "Name:  " ++ (show n)
        Real r -> "Real:  " ++ (show r)
        tk -> "Token: " ++ (show tk)

inner_format_ast [] ss = reverse ss
inner_format_ast (t:ts) ss = 
    case t of
        Success p _ -> inner_format_ast ts ((show p):ss)
        Error x s i -> inner_format_ast ts (("Error: " ++ (show x) ++ "\n\t" ++ i):ss)

format_ast ts = inner_format_ast ts []
format_token ts = map inner_format_token ts

printListLn [] = putStrLn "Done."
printListLn (ss:rs) = do
    putStrLn ss
    printListLn rs
