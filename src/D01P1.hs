module D01P1 (
    findentries
  , findentrytuple
) where

import Data.List
import Data.Maybe

findentries :: [Integer] -> Maybe Integer
findentries l = findentrytuple 2020 l >>= (Just . uncurry (*))

findentrytuple :: Integer -> [Integer] -> Maybe (Integer, Integer)
findentrytuple target l = uncons l >>= findAddends
    where
        findAddends (e, l) = maybe (findentrytuple target l) (Just . (,) e) $ find (exactSum e) l
        exactSum x y = (x + y) == target

{-
https://adventofcode.com/2020/day/1

Before you leave, the Elves in accounting just need you to fix your expense report (your puzzle input); apparently, something isn't quite adding up.

Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.

For example, suppose your expense report contained the following:

1721
979
366
299
675
1456
In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.

Of course, your expense report is much larger. Find the two entries that sum to 2020; what do you get if you multiply them together?
-}