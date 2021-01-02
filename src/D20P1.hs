module D20P1 (
    arrangePhotoEdge
  , parsePhoto
  , Tile(..)
) where

import Data.List
import Data.Tuple
import Data.Maybe
import Data.Char
import Control.Monad
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Sequence as S
import Data.Sequence ((|>), Seq(..))
import Algorithm.Search (bfs, pruning)

type Coordinate = (Int, Int)
type Pixels = M.Map Coordinate Bool
type TileID = Int
type Photo = [Tile]
type Edge = [[Int]]
type PhotoEdge = M.Map TileID [Edge]

data Tile =
    Tile {
        tPixels :: Pixels
      , tId :: TileID
      , tSize :: Int
    } deriving(Show, Eq)

arrangePhotoEdge :: Photo -> Int
arrangePhotoEdge = maybe 0 res . findEdge . toPhotoEdge
    where
        res ids = product $ map (S.index ids) [0, S.length ids - 1, sideSize ids, S.length ids - (sideSize ids) - 1]
        findEdge p =  find (validPhotoEdge p) . map S.fromList . permutations $ M.keys p
        sideSize = subtract 1 . round . sqrt . fromIntegral . S.length

validPhotoEdge :: PhotoEdge -> S.Seq TileID -> Bool
validPhotoEdge p ids = any isJust . map valid $ p ! (S.index ids 0)
    where
        valid e = bfs (nextEdge `pruning` misaligned) ((==) (S.length ids) . S.length) $ S.singleton e
        nextEdge l = map ((|>) l) $ p ! (S.index ids $ S.length l)
        misaligned l = not $ (alignedLeft l) && (alignedTop l)
        alignedLeft (xs :|> e) = (S.length xs `mod` sideSize == 0) || (maybe True (aLeft e) $ S.lookup (S.length xs - 1) xs)
        aLeft e e' = (e !! 3) == (e' !! 1)
        alignedTop (xs :|> e) = maybe True (aTop e) $ S.lookup (S.length xs - sideSize) xs
        aTop e e' = (e !! 0) == (e' !! 2)
        sideSize = round . sqrt . fromIntegral $ length p 

toPhotoEdge :: Photo -> PhotoEdge
toPhotoEdge = foldr toEdge M.empty
    where
        toEdge t = M.insert (tId t) (edgeVariants (mapEdge t) $ tSize t)
        mapEdge t = map (edge t) [(0, snd), (subtract 1 $ tSize t, fst), (subtract 1 $ tSize t, snd), (0, fst)]
        edge t (v, f) = map (f . swap) . filter ((==) v . f) . M.keys $ tPixels t

edgeVariants :: Edge -> Int -> [Edge]
edgeVariants edge size = nub $ foldr transform [edge] [flipVertical, flipHorizontal, rotate, rotate, rotate]
    where
        transform f edges = edges ++ map f edges
        rotate (a:b:c:d:[]) = [flipSide d, a, flipSide b, c]
        flipVertical (a:b:c:d:[]) = [c, flipSide b, a, flipSide d]
        flipHorizontal (a:b:c:d:[]) = [flipSide a, d, flipSide c, b]
        flipSide = reverse . map (flip subtract (size - 1))

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