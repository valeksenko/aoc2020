module Main where

import Debug.Trace

import D09P2

main :: IO ()
main = do
    -- f <- readFile "data/d06.txt"
    readFile "data/d09.txt" >>= (print . weakness 25 . map (read :: String -> Int) . lines)
    