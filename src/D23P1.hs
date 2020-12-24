module D23P1 (
    playcrabcups
) where

import Data.List
import Data.Digits

type Cup = Int

playcrabcups :: Int -> [Cup] -> Int
playcrabcups n cups = score $ foldr (\_ c -> play c) cups [1..n]
    where
        score = unDigits 10 . calcScore . break ((==) 1)
        calcScore (x, y) = (tail y) ++ x
        play (x:a:b:c:xs) = (insert [a, b, c] xs $ filter ((>) x) xs) ++ [x]
        insert c l l' = update c $ break ((==) (destination l l')) l
        update c (x, (y:ys)) = x ++ y:c ++ ys
        destination l l' = maximum $ if null l' then l else l'

{-
https://adventofcode.com/2020/day/23

The small crab challenges you to a game! The crab is going to mix up some cups, and you have to predict where they'll end up.

The cups will be arranged in a circle and labeled clockwise (your puzzle input). For example, if your labeling were 32415, there would be five cups in the circle; going clockwise around the circle from the first cup, the cups would be labeled 3, 2, 4, 1, 5, and then back to 3 again.

Before the crab starts, it will designate the first cup in your list as the current cup. The crab is then going to do 100 moves.

Each move, the crab does the following actions:

The crab picks up the three cups that are immediately clockwise of the current cup. They are removed from the circle; cup spacing is adjusted as necessary to maintain the circle.
The crab selects a destination cup: the cup with a label equal to the current cup's label minus one. If this would select one of the cups that was just picked up, the crab will keep subtracting one until it finds a cup that wasn't just picked up. If at any point in this process the value goes below the lowest value on any cup's label, it wraps around to the highest value on any cup's label instead.
The crab places the cups it just picked up so that they are immediately clockwise of the destination cup. They keep the same order as when they were picked up.
The crab selects a new current cup: the cup which is immediately clockwise of the current cup.

For example, suppose your cup labeling were 389125467.

After the crab is done, what order will the cups be in? Starting after the cup labeled 1, collect the other cups' labels clockwise into a single string with no extra characters; each number except 1 should appear exactly once. In the above example, after 10 moves, the cups clockwise from 1 are labeled 9, 2, 6, 5, and so on, producing 92658374. If the crab were to complete all 100 moves, the order after cup 1 would be 67384529.

Using your labeling, simulate 100 moves. What are the labels on the cups after cup 1?
-}