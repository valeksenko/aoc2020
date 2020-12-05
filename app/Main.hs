module Main where

import Debug.Trace

import D05P2

main :: IO ()
main = do
    f <- readFile "data/d05.txt"
    print . findseat $ lines f
