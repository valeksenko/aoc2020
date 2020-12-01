module D01P2 (
    findtripleentries
) where

import Data.List
import Data.Maybe

import D01P1

target = 2020

findtripleentries :: [Integer] -> Maybe Integer
findtripleentries l = findAddends l >>= (Just . product)
    where
        findAddends l = uncons l >>= findA
        findA (e, l) = maybe (findAddends l) (Just . asList e) $ findentrytuple (target - e) l
        asList e (x, y) = [e, x, y]
-- findentrytuple
{-
https://adventofcode.com/2020/day/1#part2

The Elves in accounting are thankful for your help; one of them even offers you a starfish coin they had left over from a past vacation. They offer you a second one if you can find three numbers in your expense report that meet the same criteria.

Using the above example again, the three entries that sum to 2020 are 979, 366, and 675. Multiplying them together produces the answer, 241861950.

In your expense report, what is the product of the three entries that sum to 2020?
-}