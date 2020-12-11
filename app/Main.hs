module Main where

import Debug.Trace

import D10P2

main :: IO ()
main = do
    -- f <- readFile "data/d06.txt"
    readFile "d" >>= (print . map (read :: String -> Int) . lines)
    readFile "d1" >>= (print . map (read :: String -> Int) . lines)
    readFile "data/d10.txt" >>= (print . adapters . map (read :: String -> Int) . lines)
    