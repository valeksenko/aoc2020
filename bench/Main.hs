module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain, env, envWithCleanup, nf)

import Bootcode
import D11P1
import D11P2

getDayInput :: String -> IO String
getDayInput day = readFile ("data/d" ++ day ++ ".txt")

inputToIntList :: String -> [Int]
inputToIntList = map read . lines

inputToIntegerList :: String -> [Integer]
inputToIntegerList = map read . lines

main :: IO ()
main = do
    let day = "11"
    area <- getDayInput day
    defaultMain
      [
        bgroup ("Day " ++ day)
            [
                bench "part 1" $ (nf D11P1.occupied area)
              , bench "part 2" $ (nf D11P2.occupied area)
            ]
      ]
