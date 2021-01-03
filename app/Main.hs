module Main where

import Debug.Trace

import D19
import D19P2

-- import D20P1
-- import D20
-- import qualified Data.Sequence as S

-- import Graphics.Gloss

-- window :: Display
-- window = InWindow "Picture" (renderSize, renderSize) (0, 0)

-- background :: Color
-- background = black

main :: IO ()
main = do
    -- f <- readFile "data/d06.txt"
    -- parseFile "d" >>= (print . flip matchMessage "aaaabb")
    -- rules <- parseFile "d.r"
    -- print rules
    -- readFile "d.m" >>= (print . length . filter id . map (matchMessage rules) . lines)
    rules <- parseFile "data/d19.rules"
    readFile "data/d19.messages" >>= (print . length . filter id . map (matchMessage rules) . lines)

    parseFile "data/d19.rules" >>= print
    -- print $ parseStatement "2 * 3 + (4 * 5)"
    -- readFile "data/d18.txt" >>= (print . sum . map (exec . parseStatement) . lines)

    -- readFile "data/d20.txt" >>= (print . arrangePhotoEdge . parsePhoto)
    -- readFile "d" >>= (print . toEdge . head . parsePhoto)
    -- readFile "d2" >>= (print . toPhotoEdge . parsePhoto)
    -- readFile "d" >>= (print . arrangePhotoEdge . parsePhoto)
    -- readFile "d2" >>= (print . flip validPhotoEdge (S.fromList [1951,2311,3079, 2729,1427,2473, 2971,1489,1171]) . toPhotoEdge . parsePhoto)
    -- print $ arrangeEdge
    --             (Edge {ePoints = [[0,2,3,7,8],[1,2,3,4,5,8],[0,4,5,7],[0,1,3,6,9]], eId = 2473, eSize = 10})
    --             (Edge {ePoints = [[0,4,5,7],[1,4,5,6,7,8],[0,2,3,7,8],[0,3,6,8,9]], eId = 2473, eSize = 10})
    -- readFile "d" >>= (display window background . renderTiles . foldr (\t a -> t:(rotate90 t):a) [] . parsePhoto)
