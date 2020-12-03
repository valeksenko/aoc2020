module D03 (
    countTrees
  , mkToboggan
  , Coordinate
  , Direction
  , Toboggan(..)
) where

import Data.List

type Coordinate = (Int, Int)
type Direction = (Int, Int)

data Toboggan =
    Toboggan {
        tPosition :: Coordinate
      , tSizeX :: Int
      , tSizeY :: Int
      , tTrees :: [Coordinate]
      , tCount :: Int
    } deriving(Show, Eq)


start = (0, 0)

countTrees :: Direction -> Toboggan -> Int
countTrees direction = tCount . until target move
    where
        target t = (snd $ tPosition t) >= (tSizeY t)
        move t = moveT t $ position (tPosition t) direction (tSizeX t)
        position (x, y) (x', y') sizeX = ((x + x') `mod` sizeX, y + y')
        moveT t p = t { tPosition = p, tCount = (amount (tTrees t) (tCount t) p) }
        amount trees c p = if any ((==) p) trees then c + 1 else c

mkToboggan :: String -> Toboggan
mkToboggan = mkT . foldl' parse ([], (0, 0), (0, 0))
    where
        mkT (t, (y, x), _) = Toboggan start (x + 1) (y + 1) t 0
        parse (t, p', p@(y, x)) c = case c of
            '.' -> (t, p, (y, x + 1))
            '#' -> ((x, y):t, p, (y, x + 1))
            '\n' -> (t, p, (y + 1, 0))