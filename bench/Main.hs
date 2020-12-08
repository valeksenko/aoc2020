module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain, env, envWithCleanup, nf)

import Bootcode
import D08P1
import D08P2

getDayInput :: String -> IO String
getDayInput day = readFile ("data/d" ++ day ++ ".txt")

inputToIntList :: String -> [Int]
inputToIntList = map read . lines

inputToIntegerList :: String -> [Integer]
inputToIntegerList = map read . lines

main :: IO ()
main = do
    code <- parseFile "data/d08.txt"
    defaultMain
      [
        bgroup "Day 8"
            [
                bench "part 1" $ (nf D08P1.execonce code)
              , bench "part 2" $ (nf D08P2.execmodified code)
            ]
      ]
