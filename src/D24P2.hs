module D24P2 (
    countblack
) where

import D24

import Data.List
import Data.Tuple.Extra
import Data.Foldable (toList)
import qualified Data.Set as S

type Limit = (Int, Int)

data State =
    State {
        sTiles :: Tiles
      , sX :: Limit
      , sY :: Limit
      , sZ :: Limit
    } deriving(Show, Eq)

countblack :: Int -> [[Move]] -> Int
countblack days = S.size . sTiles . simulate . mkState
    where
        simulate s = foldl' simulateDay s [1 .. days]

simulateDay :: State -> Int -> State
simulateDay state day = update $ foldr flipT S.empty coordinates
    where
        coordinates = [(x, y, z) | x <- (range $ sX state), y <- (range $ sY state), z <- (range $ sZ state)]
        range (l, u) = [l - day .. u + day]
        update tiles = state { sTiles = tiles }
        flipT c tiles = if (isBlack c $ sTiles state) then S.insert c tiles else tiles

mkState :: [[Move]] -> State
mkState = state . flipTiles
    where
        state tiles = State tiles (minC fst3 tiles, maxC fst3 tiles) (minC snd3 tiles, maxC snd3 tiles) (minC thd3 tiles, maxC thd3 tiles)
        minC f = minimum . map f . toList
        maxC f = maximum . map f . toList

isBlack :: Coordinate -> Tiles -> Bool
isBlack c@(x, y, z) tiles = isB (S.member c tiles) . length $ filter neighbor coordinates
    where
        coordinates = [(1, -1, 0), (1, 0, -1), (0, 1, -1), (-1, 1, 0), (-1, 0, 1), (0, -1, 1)]
        neighbor (x', y', z') = S.member (x + x', y + y', z + z') tiles
        isB True n = (n == 1) || (n == 2)
        isB False n = n == 2

{-
https://adventofcode.com/2020/day/24#part2

The tile floor in the lobby is meant to be a living art exhibit. Every day, the tiles are all flipped according to the following rules:

Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white.
Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.
Here, tiles immediately adjacent means the six tiles directly touching the tile in question.

The rules are applied simultaneously to every tile; put another way, it is first determined which tiles need to be flipped, then they are all flipped at the same time.

In the above example, the number of black tiles that are facing up after the given number of days has passed is as follows:

Day 1: 15
Day 2: 12
Day 3: 25
Day 4: 14
Day 5: 23
Day 6: 28
Day 7: 41
Day 8: 37
Day 9: 49
Day 10: 37

Day 20: 132
Day 30: 259
Day 40: 406
Day 50: 566
Day 60: 788
Day 70: 1106
Day 80: 1373
Day 90: 1844
Day 100: 2208
After executing this process a total of 100 times, there would be 2208 black tiles facing up.

How many tiles will be black after 100 days?
-}