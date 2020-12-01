module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain, env, envWithCleanup, nf)

import D01P1
import D01P2

getDayInput :: String -> IO String
getDayInput day = readFile ("data/d" ++ day ++ ".txt")

inputToIntList :: String -> [Int]
inputToIntList = map read . lines

inputToIntegerList :: String -> [Integer]
inputToIntegerList = map read . lines

main :: IO ()
main = defaultMain
  [ env (getDayInput "01") $ \input -> bgroup "Day 1"
      [ bench "part 1" $ (nf (sum . D01P1.findentries . inputToIntegerList) input)
      , bench "part 2" $ (nf (sum . D01P2.findtripleentries . inputToIntegerList) input)
      ]
  ]
