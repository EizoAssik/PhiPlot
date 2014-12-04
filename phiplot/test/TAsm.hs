module Main where

import Assembler
import System.Environment ( getArgs )

main = do
    args <- getArgs
    let fn = head args
        hexfn = fn ++ ".hex"
    src <- readFile fn
    let hex = pack $ foldl1 (++) $ map word64ToWord8 $ assembler src
    Data.ByteString.writeFile hexfn hex
        
    
