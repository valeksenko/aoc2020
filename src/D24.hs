module D24 (
    flipTiles
  , parseMoves
  , Coordinate
  , Tiles
  , Move(..)
) where

import Data.List
import qualified Data.Set as S

type Coordinate = (Int, Int, Int)
type Tiles = S.Set Coordinate

data Move
    = East
    | SouthEast
    | SouthWest
    | West
    | NorthWest
    | NorthEast
    deriving(Show, Eq)

flipTiles :: [[Move]] -> Tiles
flipTiles = foldl' flipTarget S.empty
    where
        flipTarget tiles = flipTile tiles . find
        flipTile tiles c = if S.member c tiles then S.delete c tiles else S.insert c tiles
        find = foldl' move (0, 0, 0)

parseMoves :: String -> [Move]
parseMoves input = fst $ until (null . snd) parse ([], input)
    where
        parse (m, ('e':s))     = (East:m, s)
        parse (m, ('s':'e':s)) = (SouthEast:m, s)
        parse (m, ('s':'w':s)) = (SouthWest:m, s)
        parse (m, ('w':s))     = (West:m, s)
        parse (m, ('n':'w':s)) = (NorthWest:m, s)
        parse (m, ('n':'e':s)) = (NorthEast:m, s)

move :: Coordinate -> Move -> Coordinate
move (x, y, z) m = case m of
        East      -> (x + 1, y - 1, z)
        SouthEast -> (x, y - 1 , z + 1)
        SouthWest -> (x - 1, y, z + 1)
        West      -> (x - 1, y + 1, z)
        NorthWest -> (x, y + 1, z - 1)
        NorthEast -> (x + 1, y, z - 1)
