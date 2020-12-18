module D17P2 (
    countactive
) where

import Data.List
import Data.Maybe
import qualified Data.Map as M

type Coordinate = (Int, Int, Int, Int)
type Limit = (Int, Int)
type Active = M.Map Coordinate Bool

data State =
    State {
        sActive :: Active
      , sX :: Limit
      , sY :: Limit
      , sZ :: Limit
      , sT :: Limit
    } deriving(Show, Eq)

countactive :: Int -> String -> Int
countactive steps = M.size . sActive . simulate . parseState
    where
        simulate state = foldl' simulateStep state [1 .. steps]

simulateStep :: State -> Int -> State
simulateStep state step = update $ foldr checkActive M.empty coordinates
    where
        coordinates = [(x, y, z, t) | x <- (range $ sX state), y <- (range $ sY state), z <- (range $ sZ state), t <- (range $ sT state)]
        range (l, u) = [l - step .. u + step]
        update amap = state { sActive = amap }
        checkActive c amap = if (isActive c $ sActive state) then M.insert c True amap else amap

isActive :: Coordinate -> Active -> Bool
isActive c@(cx, cy, cz, ct) amap = isA (M.member c amap) . length $ filter (flip M.member amap) coordinates
    where
        coordinates = [(x, y, z, t) | x <- [cx - 1 .. cx + 1], y <- [cy - 1 .. cy + 1], z <- [cz - 1 .. cz + 1], t <- [ct - 1 .. ct + 1], (x, y, z, t) /= c]
        isA True n = (n == 2) || (n == 3)
        isA False n = n == 3

parseState :: String -> State
parseState input = mkState . fst $ foldl' parse (M.empty, (0,0)) input
    where
        mkState amap = State amap (0, length . head $ lines input) (0, length $ lines input) (0,0) (0,0)
        parse (amap, (y, x)) c = case c of
            '#' -> (M.insert (x, y, 0, 0) True amap, (y, x + 1))
            '\n' -> (amap, (y + 1, 0))
            otherwise -> (amap, (y, x + 1))

{-
https://adventofcode.com/2020/day/17#part2

For some reason, your simulated results don't match what the experimental energy source engineers expected. Apparently, the pocket dimension actually has four spatial dimensions, not three.

The pocket dimension contains an infinite 4-dimensional grid. At every integer 4-dimensional coordinate (x,y,z,w), there exists a single cube (really, a hypercube) which is still either active or inactive.

Each cube only ever considers its neighbors: any of the 80 other cubes where any of their coordinates differ by at most 1. For example, given the cube at x=1,y=2,z=3,w=4, its neighbors include the cube at x=2,y=2,z=3,w=3, the cube at x=0,y=2,z=3,w=4, and so on.

The initial state of the pocket dimension still consists of a small flat region of cubes. Furthermore, the same rules for cycle updating still apply: during each cycle, consider the number of active neighbors of each cube.

For example, consider the same initial state as in the example above. Even though the pocket dimension is 4-dimensional, this initial state represents a small 2-dimensional slice of it. (In particular, this initial state defines a 3x3x1x1 region of the 4-dimensional space.)

Simulating a few cycles from this initial state produces the following configurations, where the result of each cycle is shown layer-by-layer at each given z and w coordinate:

After the full six-cycle boot process completes, 848 cubes are left in the active state.

Starting with your given initial configuration, simulate six cycles in a 4-dimensional space. How many cubes are left in the active state after the sixth cycle?
-}