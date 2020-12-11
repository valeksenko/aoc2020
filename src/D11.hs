module D11 (
    parseArea
  , Coordinate
  , AreaMap
  , Tile(..)
) where

import Data.List
import qualified Data.Map as M

type Coordinate = (Int, Int)
type AreaMap = M.Map Coordinate Tile

data Tile
    = Occupied
    | Empty
    | Floor
    deriving(Show, Eq, Ord)

parseArea :: String -> AreaMap
parseArea = fst . foldl' parse (M.empty, (0,0))
    where
        parse (amap, (y, x)) c = case c of
            '.' -> (M.insert (x, y) Floor amap, (y, x + 1))
            'L' -> (M.insert (x, y) Empty amap, (y, x + 1))
            '\n' -> (amap, (y + 1, 0))
            otherwise -> (amap, (y, x + 1))
