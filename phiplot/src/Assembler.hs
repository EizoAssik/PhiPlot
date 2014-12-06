module Assembler where

import Data.Word
import Data.Bits
import Data.List ( findIndex )
import Data.ByteString ( ByteString, pack, writeFile )
import Data.Binary.IEEE754 ( doubleToWord, wordToDouble )
import System.Environment ( getArgs )

keys = [
    ("NOP",  0x00),
    ("POP",  0x01),
    ("PUSH", 0x02),
    ("SWAP", 0x03),
    ("DUP",  0x04),
    ("ADD",  0x05),
    ("SUB",  0x06),
    ("MUL",  0x07),
    ("DIV",  0x08),
    ("CALL", 0x09),
    ("RET",  0x0A),
    ("NEG",  0x0B),
    ("AND",  0x0C),
    ("OR",   0x0D),
    ("JMP",  0x0E),
    ("JP",   0x0F),
    ("JZ",   0x10),
    ("LT",   0x11),
    ("EQ",   0x12),
    ("GT",   0x13),
    ("NOT",  0x14),
    ("DRAW", 0x15),
    ("HALT", 0x16),
    ("STORE",0x17),
    ("LOAD", 0x18),
    ("SIN",  0x19),
    ("COS",  0x1A),
    ("TAN",  0x1B) ] :: [(String, Word64)]

word64ToWord8 :: Word64 -> [Word8]
word64ToWord8 raw = 
    map (\x -> fromIntegral $ (.&. 0xFF) $ shiftR raw x ) [0,8..56]

findKnownCode name = 
    case lookup name keys of
        Just code -> code
        Nothing   -> 0x00

dumpDouble literal = 
    doubleToWord $ (read literal :: Double)

dumpWord64 literal = 
    (read literal :: Word64)

asm [] = []

asm (x:literal:xs)
    | x == "PUSH" = (findKnownCode x):(dumpDouble literal):(asm xs)
    | elem x ["PUSH", "JMP", "JP", "JZ", "CALL"] = 
        (findKnownCode x):(dumpWord64 literal):(asm xs)

asm (x:xs) = 
    case lookup x keys of
        Just hex -> hex:(asm xs)
        Nothing  -> []

removeComment line = 
    case findIndex (==';') line of
        Nothing -> line
        Just i  -> take i line

removeComments src = 
    foldl1 (\x y -> x++(' ':y)) $ map removeComment $ lines src
    
tokenize src = words $ removeComments src

assembler src = asm $ tokenize src
