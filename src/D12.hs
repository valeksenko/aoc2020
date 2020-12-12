module D12 (
    parseMove
  , Coordinate
  , Move(..)
) where

import Data.List

type Coordinate = (Int, Int)

data Move
    = GoEast Int
    | GoWest Int
    | GoNorth Int
    | GoSouth Int
    | TurnLeft Int
    | TurnRight Int
    | Forward Int
    deriving(Show, Eq, Ord)


parseMove :: String -> [Move]
parseMove = map move . lines
    where
        move (m:n) = (parse m) (read n :: Int)
        parse m = case m of    
            'N' -> GoNorth
            'E' -> GoEast
            'S' -> GoSouth
            'W' -> GoWest
            'L' -> TurnLeft
            'R' -> TurnRight
            'F' -> Forward
