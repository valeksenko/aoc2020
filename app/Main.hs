module Main where

import Debug.Trace

-- import D19
-- import D19P1

import D22P2

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
    -- rules <- parseFile "data/d19.rules"
    -- readFile "data/d19.messages" >>= (print . length . filter id . map (matchMessage rules) . lines)

    -- parseFile "data/d19.rules" >>= print
    -- print $ parseStatement "2 * 3 + (4 * 5)"
    -- readFile "data/d18.txt" >>= (print . sum . map (exec . parseStatement) . lines)

    -- readFile "data/d20.txt" >>= (print . arrangePhoto . parsePhoto)
    -- readFile "d" >>= (print . arrangePhotoEdge . parsePhoto)
    print $ playcombat ([9, 2, 6, 3, 1], [5, 8, 4, 7, 10])
    print $ playcombat ([10, 39, 16, 32, 5, 46, 47, 45, 48, 26, 36, 27, 24, 37, 49, 25, 30, 13, 23, 1, 9, 3, 31, 14, 4], [2, 15, 29, 41, 11, 21, 8, 44, 38, 19, 12, 20, 40, 17, 22, 35, 34, 42, 50, 6, 33, 7, 18, 28, 43])
    -- readFile "d" >>= (display window background . renderTiles . foldr (\t a -> t:(rotate90 t):a) [] . parsePhoto)
 
