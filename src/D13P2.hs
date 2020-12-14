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

bruteforcetimestamp :: [Schedule] -> Integer
bruteforcetimestamp schedules = foldl' tstamp (simpletimestamp $ take 2 schedules) $ drop 2 schedules
    where
        tstamp s (d, b) = simpletimestamp $ traceShowId [(0, s), (1, b)]

-- bruteforcetimestamp :: Integer -> [Schedule] -> Integer
-- bruteforcetimestamp start schedules = fst $ timestamp schedules (start, 571)

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

