module Main where

import Debug.Trace

import D03P1

main :: IO ()
main = do
    f <- readFile "data/d03.txt"
    print . traveltoboggan $ mkToboggan f
