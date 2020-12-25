module Main where

import Debug.Trace

-- import D19
-- import D19P1

import D21
import D21P2

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
    -- readFile "d" >>= (display window background . renderTiles . foldr (\t a -> t:(rotate90 t):a) [] . parsePhoto)
    -- print $ playcrabcups 1000000 [3,8,9,1,2,5,4,6,7]

    -- print $ playcrabcups 100 [3,8,9,1,2,5,4,6,7]
    -- print $ playcrabcups 100 [3,2,7,4,6,5,1,8,9]
 
    readFile "d" >>= (print . allergens . map parseIngredientList . lines)
    readFile "d" >>= (print . map parseIngredientList . lines)
    readFile "data/d21.txt" >>= (print . allergens . map parseIngredientList . lines)