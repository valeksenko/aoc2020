module Main where

import Debug.Trace

import D15

main :: IO ()
main = do
    -- f <- readFile "data/d06.txt"
    -- parseFile "data/d14.txt" >>= print
    print $ spokennumber 30000000 [8,0,17,4,1,12]
    -- print $ spoken 30000000 [8,0,17,4,1,12]
    -- parseFile "d" >>= print
