module D21 (
    mapAllergens
  , parseIngredientList
  , IngredientList
) where

import Data.List
import Data.List.Split
import qualified Data.Map as M

type Ingredient = String
type Allergen = String
type IngredientList = ([Ingredient], [Allergen])
type AllergenMap = M.Map Allergen Ingredient

mapAllergens :: [IngredientList] -> AllergenMap
mapAllergens ingredients = mapA . map onAllergen . nub $ concatMap snd ingredients
    where
        onAllergen a = (a, map fst $ filter (elem a . snd) ingredients)
        mapA al = snd $ until (null . fst) checkAllergen (al, M.empty)
        checkAllergen (al, amap) = foldr checkA ([], amap) al
        checkA (a, is) (al, amap) = match al amap a is . foldr1 intersect $ map (flip (\\) (M.elems amap)) is
        match al amap a is l = if length l == 1 then (al, M.insert a (head l) amap) else ((a, is):al, amap)

parseIngredientList :: String -> IngredientList
parseIngredientList = asList . splitOn " (contains "
    where
        asList (i:a:[]) = (words i, splitOn ", " $ init a)
