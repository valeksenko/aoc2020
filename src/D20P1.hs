module D20P1 (
    arrangePhoto
  , arrangePhotoEdge
  , parsePhoto
  , toEdge
  , Tile(..)
) where

import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.Sequence((|>))

import Debug.Trace

type Coordinate = (Int, Int)
type Pixels = M.Map Coordinate Bool
type TileID = Int
type Photo = [Tile]
type PhotoEdge = [Edge]

data Tile =
    Tile {
        tPixels :: Pixels
      , tId :: TileID
      , tSize :: Int
    } deriving(Show, Eq)

data Edge =
    Edge {
        ePoints :: [[Int]]
      , eId :: TileID
      , eSize :: Int
    } deriving(Show, Eq)


arrangePhoto :: Photo -> Int -- Maybe Photo
arrangePhoto = length . allPhotos . permutations
    where
        allPhotos edges = foldl' transform edges [flipVertical, rotate90, rotate180, rotate270]
        transform edges f = edges ++ map (map f) edges

arrangePhotoEdge :: Photo -> Int
arrangePhotoEdge = maybe 0 res . find validPhotoEdge . allEdges . permutations . map toEdge
    where
        res edges = product $ map (eId . (!!) edges) [0, length edges - 1, size edges, length edges - size edges - 1]
        size edges = (round . sqrt . fromIntegral $ length edges) - 1
        allEdges edges = foldl' transform edges [flipVertical, flipHorizontal, rotate 1, rotate 2, rotate 3]
        transform edges f = edges ++ map (map f) edges
        flipVertical (Edge (a:b:c:d:[]) tid s) = Edge [d, flipSide s b, flipSide s c, a] tid s
        flipHorizontal (Edge (a:b:c:d:[]) tid s) = Edge [flipSide s a, c, b, flipSide s d] tid s
        flipSide s = map (flip subtract (s - 1))
        rotate n e =  e { ePoints = (drop n . take (4 + n) . cycle $ ePoints e) }

validPhotoEdge :: PhotoEdge -> Bool
validPhotoEdge edges = valid $ map (\y -> map (\x -> y*limit + x) [0..limit]) [0..limit]
    where
        limit = (round . sqrt . fromIntegral $ length edges) - 1
        edge = ePoints . (!!) edges
        valid indices = (all matchX indices) && (all matchY $ transpose indices)
        matchX indices = all mX $ zip (tail indices) indices
        mX (i', i) = (edge i !! 1) == (edge i' !! 3)
        matchY indices = all mY $ zip (tail indices) indices
        mY (i', i) = (edge i !! 2) == (edge i' !! 0)

toEdge :: Tile -> Edge
toEdge t = Edge mapEdge (tId t) (tSize t)
    where
        mapEdge = map edge [(0, snd), (subtract 1 $ tSize t, fst), (subtract 1 $ tSize t, snd), (0, fst)]
        edge (v, f) = map f . filter ((==) v . f) . M.keys $ tPixels t

rotate90 :: Tile -> Tile
rotate90 tile = adjustTile (\(x, y) -> (y, (tSize tile) - x)) tile

rotate180 :: Tile -> Tile
rotate180 tile = adjustTile (\(x, y) -> ((tSize tile) - x, (tSize tile) - y)) tile

rotate270 :: Tile -> Tile
rotate270 tile = adjustTile (\(x, y) -> ((tSize tile) - y, x)) tile

flipVertical :: Tile -> Tile
flipVertical = adjustTile (\(x, y) -> (negate x, y))

adjustTile :: (Coordinate -> Coordinate) -> Tile -> Tile
adjustTile f tile = tile { tPixels = adjustK $ tPixels tile }
    where
        adjustK = M.foldrWithKey (M.insert . f) M.empty

parsePhoto :: String -> Photo
parsePhoto = foldl' parse [] . lines
    where
        parse p "" = p
        parse p ('T':l) = (Tile M.empty (read (filter isDigit l) :: TileID) 0):p
        parse (t:p) l = (parseTile t l):p

parseTile :: Tile -> String -> Tile
parseTile tile = update . fst . foldl' parse (tPixels tile, (tSize tile, 0))
    where
        update pixels = Tile pixels (tId tile) (1 + tSize tile)
        parse (pixels, (y, x)) c = case c of
            '#' -> (M.insert (x, y) True pixels, (y, x + 1))
            otherwise -> (pixels, (y, x + 1))
{-
https://adventofcode.com/2020/day/20

-}