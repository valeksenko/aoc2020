{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace

import D14
import D14P2

main :: IO ()
main = do
    -- f <- readFile "data/d06.txt"
    -- parseFile "data/d14.txt" >>= print
    parseFile "data/d14.txt" >>= (print . sumval)
    print $ sumval [Mask "000000000000000000000000000000X1001X",Mem (42,100),Mask "00000000000000000000000000000000X0XX",Mem (26,1)]
    -- parseFile "d" >>= print
