{-# LANGUAGE ScopedTypeVariables  #-}

module D13P2 (
    simpletimestamp
  , bruteforcetimestamp
  , svbtimestamp
  , parseSchedule
) where

import Debug.Trace
import Data.List
import Data.Ord
import Data.Tuple.Extra
import Data.List.Split
import Data.SBV
import Data.SBV.Tuple

type Schedule = (Integer, Integer)

timestamp :: [Schedule] -> (Integer, Integer) -> (Integer, Integer)
timestamp schedules = until found next
    where
        next (s, b) = (s + b, b)
        found (s, _) = all (startOn s) schedules
        startOn s (d, b) = (s + d) `mod` b == 0

simpletimestamp :: [Schedule] -> Integer
simpletimestamp schedules = fst . timestamp schedules . start $ maximumBy (comparing snd) schedules
    where
        start (d, b) = (b - d, b)

-- bruteforcetimestamp :: [Schedule] -> Integer
-- bruteforcetimestamp schedules = foldl' tstamp (simpletimestamp $ take 2 schedules) $ drop 2 schedules
--     where
--         tstamp s (d, b) = simpletimestamp $ traceShowId [(0, s), (1, b)]

bruteforcetimestamp :: Integer -> [Schedule] -> Integer
bruteforcetimestamp start schedules = fst $ timestamp schedules (start, 571)

svbtimestamp :: IO Integer
svbtimestamp = do
    LexicographicResult model <- optimize Lexicographic $ findEarliest
    case "start" `getModelValue` model of
        Just d -> return d
        Nothing -> error "Unsolvable"

findEarliest :: Goal
findEarliest = do
        -- bus :: STuple Integer Integer <- sTuple "BUS"
        start <- sInteger "START"

        constrain $ start .> 100000000000000

        -- constrain $ sAll (\(d, b) -> (start + literal d) `sMod` (literal b) .== 0) schedules
        constrain $ (start + 0) `sMod` 17 .== 0
        constrain $ (start + 11) `sMod` 37 .== 0
        constrain $ (start + 17) `sMod` 571 .== 0
        constrain $ (start + 35) `sMod` 13 .== 0
        constrain $ (start + 40) `sMod` 23 .== 0
        constrain $ (start + 46) `sMod` 29 .== 0
        constrain $ (start + 48) `sMod` 401 .== 0
        constrain $ (start + 58) `sMod` 41 .== 0
        constrain $ (start + 67) `sMod` 19 .== 0

        minimize "start" $ start

-- svbtimestamp :: [Schedule] -> IO Integer
-- svbtimestamp schedules = do
--     LexicographicResult model <- optimize Lexicographic $ findEarliest schedules
--     case "start" `getModelValue` model of
--         Just d -> return d
--         Nothing -> error "Unsolvable"

-- findEarliest :: [Schedule] -> Goal
-- findEarliest schedules = do
--         -- bus :: STuple Integer Integer <- sTuple "BUS"
--         start <- sInteger "START"

--         constrain $ start .> 100000000000000
--         constrain $ sAll (\(d, b) -> (start + literal d) `sMod` (literal b) .== 0) schedules
--         -- constrain $ (start + (bus^._1)) `sMod` (bus^._2) .== 0

--         minimize "start" $ start

parseSchedule :: String -> [Schedule]
parseSchedule = map (second (read :: String -> Integer)) . filter ((/=) "x" . snd) . zip [0..] . splitOn ","


        -- let
        --     isNeighbor (p, radius) = literal radius .>= distance p
        --     distance (x', y', z') = calcD x x' + calcD y y' + calcD z z'
        --     calcD n n' = sAbs $ n - (literal n')
        --     sAbs n = ite (n .< 0) (-n) n
        --     neighborCnt :: SInteger
        --     neighborCnt = sum $ map (oneIf . isNeighbor) positions

        -- maximize "neighborCount" $ neighborCnt
        -- minimize "distance" $ distance (0, 0, 0)

{-
https://adventofcode.com/2020/day/13#part2

The shuttle company is running a contest: one gold coin for anyone that can find the earliest timestamp such that the first bus ID departs at that time and each subsequent listed bus ID departs at that subsequent minute. (The first line in your input is no longer relevant.)

For example, suppose you have the same list of bus IDs as above:

7,13,x,x,59,x,31,19
An x in the schedule means there are no constraints on what bus IDs must depart at that time.

This means you are looking for the earliest timestamp (called t) such that:

Bus ID 7 departs at timestamp t.
Bus ID 13 departs one minute after timestamp t.
There are no requirements or restrictions on departures at two or three minutes after timestamp t.
Bus ID 59 departs four minutes after timestamp t.
There are no requirements or restrictions on departures at five minutes after timestamp t.
Bus ID 31 departs six minutes after timestamp t.
Bus ID 19 departs seven minutes after timestamp t.
The only bus departures that matter are the listed bus IDs at their specific offsets from t. Those bus IDs can depart at other times, and other bus IDs can depart at those times. For example, in the list above, because bus ID 19 must depart seven minutes after the timestamp at which bus ID 7 departs, bus ID 7 will always also be departing with bus ID 19 at seven minutes after timestamp t.

In this example, the earliest timestamp at which this occurs is 1068781:

In the above example, bus ID 7 departs at timestamp 1068788 (seven minutes after t). This is fine; the only requirement on that minute is that bus ID 19 departs then, and it does.

Here are some other examples:

The earliest timestamp that matches the list 17,x,13,19 is 3417.
67,7,59,61 first occurs at timestamp 754018.
67,x,7,59,61 first occurs at timestamp 779210.
67,7,x,59,61 first occurs at timestamp 1261476.
1789,37,47,1889 first occurs at timestamp 1202161486.
However, with so many bus IDs in your list, surely the actual earliest timestamp will be larger than 100000000000000!

What is the earliest timestamp such that all of the listed bus IDs depart at offsets matching their positions in the list?
-}