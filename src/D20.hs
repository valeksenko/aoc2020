module D20 (
    renderSize
  , renderTile
  , renderTiles
  , Tile(..)
) where

import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Map as M
import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.ViewPort

import Debug.Trace

type Coordinate = (Int, Int)
type Pixels = M.Map Coordinate Bool
type TileID = Int
type Photo = [Tile]

data Tile =
    Tile {
        tPixels :: Pixels
      , tId :: TileID
      , tSize :: Int
    } deriving(Show, Eq)


renderSize = 900 :: Int
psize = 20

renderTiles :: [Tile] -> Picture
renderTiles = pictures . map renderT . zip [0..]
    where
        renderT (n, t) = renderTile (n * 1.5 * psize * (fromIntegral $ tSize t)) t

renderTile :: Float -> Tile -> Picture
-- renderTile tile = pictures $ [tid, outline] ++ pixels
renderTile offset tile = pictures pixels
    where
        tid = translate (-280) 200 $ scale 0.2 0.2 $ color white $ text $ "Tile: " ++ (show $ tId tile)
        outline = translate (-40) 0 $ color red $ rectangleWire tsize tsize
        tsize = (fromIntegral $ tSize tile) * psize + 20
        pixels = map pixel . M.keys $ tPixels tile
        pixel (x, y) = translate (offset + psize * (fromIntegral x)) (offset + psize * (fromIntegral y)) $ color green $ rectangleSolid psize psize
