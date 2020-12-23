module D22P1 (
    playcombat
) where

import Data.List

playcombat :: ([Int], [Int]) -> Int
playcombat = score . until winner play
    where
        winner (x, y) = null x || null y
        play ((x:xs), (y:ys)) = if x > y then (xs ++ [x, y], ys) else (xs, ys ++ [y, x])
        score (x, y) = sum . map (uncurry (*)) . zip [1..] . reverse $ if null x then y else x

{-
https://adventofcode.com/2020/day/21

-}