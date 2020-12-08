module Main where

import Debug.Trace
import Bootcode
import D08P2

main :: IO ()
main = do
    -- f <- readFile "data/d06.txt"
    parseFile "data/d08.txt" >>= (print . execmodified)
    