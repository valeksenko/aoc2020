module D21P2 (
    allergens
) where

import D21

import Data.List
import qualified Data.Map as M

allergens :: [IngredientList] -> String
allergens = intercalate "," . map snd . sortOn fst . M.foldrWithKey tuples [] . mapAllergens
    where
        tuples a i = (:) (a, i)

{-
https://adventofcode.com/2020/day/21#part2

Now that you've isolated the inert ingredients, you should have enough information to figure out which ingredient contains which allergen.

In the above example:

mxmxvkd contains dairy.
sqjhc contains fish.
fvjkl contains soy.
Arrange the ingredients alphabetically by their allergen and separate them by commas to produce your canonical dangerous ingredient list. (There should not be any spaces in your canonical dangerous ingredient list.) In the above example, this would be mxmxvkd,sqjhc,fvjkl.

Time to stock your raft with supplies. What is your canonical dangerous ingredient list?
-}