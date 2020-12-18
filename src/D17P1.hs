module D17P1 (
    countactive
) where

import Data.List
import Data.Maybe
import qualified Data.Map as M

import Debug.Trace

type Coordinate = (Int, Int, Int)
type Limit = (Int, Int)
type Active = M.Map Coordinate Bool

data State =
    State {
        sActive :: Active
      , sX :: Limit
      , sY :: Limit
      , sZ :: Limit
    } deriving(Show, Eq)

countactive :: Int -> String -> Int
countactive steps = M.size . sActive . simulate . traceShowId . parseState
    where
        simulate state = foldl' simulateStep state [1 .. steps]

simulateStep :: State -> Int -> State
simulateStep state step = update $ foldr checkActive M.empty coordinates
    where
        coordinates = [(x, y, z) | x <- (range $ sX state), y <- (range $ sY state), z <- (range $ sZ state)]
        range (l, u) = [l - step .. u + step]
        update amap = state { sActive = amap }
        checkActive c amap = if (isActive c $ sActive state) then M.insert c True amap else amap

isActive :: Coordinate -> Active -> Bool
isActive c@(cx, cy, cz) amap = isA (M.member c amap) . length $ filter (flip M.member amap) coordinates
    where
        coordinates = [(x, y, z) | x <- [cx - 1 .. cx + 1], y <- [cy - 1 .. cy + 1], z <- [cz - 1 .. cz + 1], (x, y, z) /= c]
        isA True n = (n == 2) || (n == 3)
        isA False n = n == 3

parseState :: String -> State
parseState input = mkState . fst $ foldl' parse (M.empty, (0,0)) input
    where
        mkState amap = State amap (0, length . head $ lines input) (0, length $ lines input) (0,0)
        parse (amap, (y, x)) c = case c of
            '#' -> (M.insert (x, y, 0) True amap, (y, x + 1))
            '\n' -> (amap, (y + 1, 0))
            otherwise -> (amap, (y, x + 1))

{-
https://adventofcode.com/2020/day/17

The pocket dimension contains an infinite 3-dimensional grid. At every integer 3-dimensional coordinate (x,y,z), there exists a single cube which is either active or inactive.

In the initial state of the pocket dimension, almost all cubes start inactive. The only exception to this is a small flat region of cubes (your puzzle input); the cubes in this region start in the specified active (#) or inactive (.) state.

The energy source then proceeds to boot up by executing six cycles.

Each cube only ever considers its neighbors: any of the 26 other cubes where any of their coordinates differ by at most 1. For example, given the cube at x=1,y=2,z=3, its neighbors include the cube at x=2,y=2,z=2, the cube at x=0,y=2,z=3, and so on.

During a cycle, all cubes simultaneously change their state according to the following rules:

If a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active. Otherwise, the cube becomes inactive.
If a cube is inactive but exactly 3 of its neighbors are active, the cube becomes active. Otherwise, the cube remains inactive.
The engineers responsible for this experimental energy source would like you to simulate the pocket dimension and determine what the configuration of cubes should be at the end of the six-cycle boot process.

For example, consider the following initial state:

.#.
..#
###
Even though the pocket dimension is 3-dimensional, this initial state represents a small 2-dimensional slice of it. (In particular, this initial state defines a 3x3x1 region of the 3-dimensional space.)

After the full six-cycle boot process completes, 112 cubes are left in the active state.

Starting with your given initial configuration, simulate six cycles. How many cubes are left in the active state after the sixth cycle?
-}