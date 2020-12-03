module Main where

import Debug.Trace

import D03
import D03P2

main :: IO ()
main = do
    f <- readFile "data/d03.txt"
    print . travelslopes $ mkToboggan f
