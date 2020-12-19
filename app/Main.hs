module Main where

import Debug.Trace

import D18
import D18P2

main :: IO ()
main = do
    -- f <- readFile "data/d06.txt"
    -- parseFile "data/d14.txt" >>= print
    print . exec $ parseStatement "2 * 3 + (4 * 5)"
    -- print $ parseStatement "2 * 3 + (4 * 5)"
    readFile "data/d18.txt" >>= (print . sum . map (exec . parseStatement) . lines)
