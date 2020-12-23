module D21P1 (
    parseIngredientList
) where

import Data.List
import Data.List.Split

type Ingredient = String
type Allergen = String
type IngredientList = ([Ingredient], [Allergen])

parseIngredientList :: String -> IngredientList
parseIngredientList = asList . splitOn " (contains "
    where
        asList (i:a:[]) = (words i, splitOn ", " $ init a)

{-
https://adventofcode.com/2020/day/21

-}