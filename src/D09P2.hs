module D09P2 (
    weakness
) where

import Data.List
import D09P1

weakness :: Int -> [Int] -> Int
weakness amount numbers = result . fst . findWeakness $ badnumber amount numbers
    where
        findWeakness bad = until (weaknessFound bad) (nextInput bad) $ nextInput bad ([], numbers)
        weaknessFound bad (l, _) = sum l == bad
        nextInput bad (_, l) = (takeChunk bad l, tail l)
        takeChunk bad l = until (found bad (length l)) (nextChunk l) $ take 2 l
        nextChunk l l' = take (length l' + 1) l
        result l = (minimum l) + (maximum l)
        found n size l
            | (sum l) == n = True
            | (sum l) > n  = True
            | length l == size = True
            | otherwise = False


{-
https://adventofcode.com/2020/day/9#part2

The final step in breaking the XMAS encryption relies on the invalid number you just found: you must find a contiguous set of at least two numbers in your list which sum to the invalid number from step 1.

Again consider the above example:

35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576
In this list, adding up all of the numbers from 15 through 40 produces the invalid number from step 1, 127. (Of course, the contiguous set of numbers in your actual list might be much longer.)

To find the encryption weakness, add together the smallest and largest number in this contiguous range; in this example, these are 15 and 47, producing 62.

What is the encryption weakness in your XMAS-encrypted list of numbers?
-}