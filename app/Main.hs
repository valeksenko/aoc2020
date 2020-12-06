module Main where

import Debug.Trace

import D06P2

main :: IO ()
main = do
    f <- readFile "data/d06.txt"
    print . groupquestions $ lines f
