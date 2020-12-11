module Main where

import Debug.Trace

import D11
import D11P1

main :: IO ()
main = do
    -- f <- readFile "data/d06.txt"
    readFile "data/d11.txt" >>= (print . occupied . parseArea)
    