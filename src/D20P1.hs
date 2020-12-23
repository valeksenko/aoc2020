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

The high-speed train leaves the forest and quickly carries you south. You can even see a desert in the distance! Since you have some spare time, you might as well see if there was anything interesting in the image the Mythical Information Bureau satellite captured.

After decoding the satellite messages, you discover that the data actually contains many small images created by the satellite's camera array. The camera array consists of many cameras; rather than produce a single square image, they produce many smaller square image tiles that need to be reassembled back into a single image.

Each camera in the camera array returns a single monochrome image tile with a random unique ID number. The tiles (your puzzle input) arrived in a random order.

Worse yet, the camera array appears to be malfunctioning: each image tile has been rotated and flipped to a random orientation. Your first task is to reassemble the original image by orienting the tiles so they fit together.

To show how the tiles should be reassembled, each tile's image data includes a border that should line up exactly with its adjacent tiles. All tiles have this border, and the border lines up exactly when the tiles are both oriented correctly. Tiles at the edge of the image also have this border, but the outermost edges won't line up with any other tiles.

For example, suppose you have the following nine tiles:

Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...
By rotating, flipping, and rearranging them, you can find a square arrangement that causes all adjacent borders to line up:

#...##.#.. ..###..### #.#.#####.
..#.#..#.# ###...#.#. .#..######
.###....#. ..#....#.. ..#.......
###.##.##. .#.#.#..## ######....
.###.##### ##...#.### ####.#..#.
.##.#....# ##.##.###. .#...#.##.
#...###### ####.#...# #.#####.##
.....#..## #...##..#. ..#.###...
#.####...# ##..#..... ..#.......
#.##...##. ..##.#..#. ..#.###...

#.##...##. ..##.#..#. ..#.###...
##..#.##.. ..#..###.# ##.##....#
##.####... .#.####.#. ..#.###..#
####.#.#.. ...#.##### ###.#..###
.#.####... ...##..##. .######.##
.##..##.#. ....#...## #.#.#.#...
....#..#.# #.#.#.##.# #.###.###.
..#.#..... .#.##.#..# #.###.##..
####.#.... .#..#.##.. .######...
...#.#.#.# ###.##.#.. .##...####

...#.#.#.# ###.##.#.. .##...####
..#.#.###. ..##.##.## #..#.##..#
..####.### ##.#...##. .#.#..#.##
#..#.#..#. ...#.#.#.. .####.###.
.#..####.# #..#.#.#.# ####.###..
.#####..## #####...#. .##....##.
##.##..#.. ..#...#... .####...#.
#.#.###... .##..##... .####.##.#
#...###... ..##...#.. ...#..####
..#.#....# ##.#.#.... ...##.....
For reference, the IDs of the above tiles are:

1951    2311    3079
2729    1427    2473
2971    1489    1171
To check that you've assembled the image correctly, multiply the IDs of the four corner tiles together. If you do this with the assembled tiles from the example above, you get 1951 * 3079 * 2971 * 1171 = 20899048083289.

Assemble the tiles into an image. What do you get if you multiply together the IDs of the four corner tiles?
-}