module Main where

import Debug.Trace

import D19
import D19P1

main :: IO ()
main = do
    -- f <- readFile "data/d06.txt"
    -- parseFile "d" >>= (print . flip matchMessage "aaaabb")
    rules <- parseFile "d.r"
    print rules
    readFile "d.m" >>= (print . length . filter id . map (matchMessage rules) . lines)
    -- rules <- parseFile "data/d19.rules"
    -- readFile "data/d19.messages" >>= (print . length . filter id . map (matchMessage rules) . lines)

    -- parseFile "data/d19.rules" >>= print
    -- print $ parseStatement "2 * 3 + (4 * 5)"
    -- readFile "data/d18.txt" >>= (print . sum . map (exec . parseStatement) . lines)
