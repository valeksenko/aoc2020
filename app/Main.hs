module Main where

import Debug.Trace

import D17P2

main :: IO ()
main = do
    -- f <- readFile "data/d06.txt"
    -- parseFile "data/d14.txt" >>= print
    readFile "d" >>= (print . countactive 6)
    readFile "data/d17.txt" >>= (print . countactive 6)
