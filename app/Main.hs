module Main where

import Debug.Trace

import D12
import D12P2

main :: IO ()
main = do
    -- f <- readFile "data/d06.txt"
    readFile "data/d12.txt" >>= (print . distance . parseMove)
    