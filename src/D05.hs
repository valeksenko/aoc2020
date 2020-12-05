module D05 (
    scanPasses
  , BoardPass
  , Seat
) where

import Data.List
import Data.Tuple
import Data.Tuple.Extra

type BoardPass = String
type Seat = Int

scanPasses :: [BoardPass] -> [Seat]
scanPasses = map seat
    where
        seat = uncurry (+) . first ((*) 8) . locateSeat . splitAt 7
        locateSeat (r, c) = (row r, column c)

row :: String -> Int
row = fst . foldl' apply (0, 127) . zip [2^x | x <- [6,5..]] . map half
    where
        apply (x, y) (n, (x', y')) = (x + n*x', y + n*y')
        half c = case c of
            'F' -> (0, (-1))
            'B' -> (1, 0)

column :: String -> Int
column = fst . foldl' apply (0, 7) . zip [2^x | x <- [2,1..]] . map half
    where
        apply (x, y) (n, (x', y')) = (x + n*x', y + n*y')
        half c = case c of
            'L' -> (0, (-1))
            'R' -> (1, 0)
