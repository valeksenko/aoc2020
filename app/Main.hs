module Main where

import Debug.Trace

import D13P2

main :: IO ()
main = do
    -- f <- readFile "data/d06.txt"
    -- readFile "data/d12.txt" >>= (print . distance . parseMove)
    -- (svbtimestamp $ parseSchedule "17,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,571,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,13,x,x,x,x,23,x,x,x,x,x,29,x,401,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,19") >>= print
    -- svbtimestamp >>= print
    -- print . bruteforcetimestamp 808570074979 $ parseSchedule "17,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,571,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,13,x,x,x,x,23,x,x,x,x,x,29,x,401,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,19"
    -- print . bruteforcetimestamp 100000000000000 $ parseSchedule "17,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,571,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,13,x,x,x,x,23,x,x,x,x,x,29,x,401,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,19"
    print . bruteforcetimestamp . traceShowId $ parseSchedule "7,13,x,x,59,x,31,19"
    -- print $ parseSchedule