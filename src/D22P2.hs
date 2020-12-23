module D22P2 (
    playcombat
) where

import Data.List

playcombat :: ([Int], [Int]) -> Int
playcombat = score . play
    where
        score (x, y) = sum . map (uncurry (*)) . zip [1..] . reverse $ if null x then y else x

play :: ([Int], [Int]) -> ([Int], [Int])
play = fst . until finished round . newGame
    where
        newGame c = (c, [])
        finished ((x, y), _) = null x || null y
        round ((xx@(x:xs), yy@(y:ys)), p)
            | (any ((==) (xx, yy)) p) = ((xx, []), [])
            | (length xs >= x) && (length ys >= y) = if (null . fst $ play (xs, ys)) then ((xs, ys ++ [y, x]), (xx, yy):p) else ((xs ++ [x, y], ys), (xx, yy):p)
            | otherwise = if x > y then ((xs ++ [x, y], ys), (xx, yy):p) else ((xs, ys ++ [y, x]), (xx, yy):p)

{-
https://adventofcode.com/2020/day/21

-}