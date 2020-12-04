module Main where

import Debug.Trace

import D04
import D04P2

main :: IO ()
main = do
    f <- readFile "data/d04.txt"
    print . validdocs $ mkDocuments f
