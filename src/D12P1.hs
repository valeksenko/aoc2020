module D12P1 (
    distance
) where

import Data.List
import D12

type State = (Direction, Coordinate)

data Direction
    = East
    | West
    | North
    | South

start :: State
start = (East, (0, 0))

distance :: [Move] -> Int
distance = distance (snd start) . snd . foldl' makeMove start
    where
        distance (x, y) (x', y') = abs (x + x') + abs (y + y')

makeMove :: State -> Move -> State
makeMove (d, (x, y)) (GoEast n) = (d, (x, y + n))
makeMove (d, (x, y)) (GoWest n) = (d, (x, y - n))
makeMove (d, (x, y)) (GoNorth n) = (d, (x + n, y))
makeMove (d, (x, y)) (GoSouth n) = (d, (x - n, y))
makeMove s@(d, c) (Forward n) = makeMove s (forward d n)
makeMove (d, c) (TurnLeft n) = (newDirection (-n) d, c)
makeMove (d, c) (TurnRight n) = (newDirection n d, c)

newDirection :: Int -> Direction -> Direction
newDirection degree = toDirection . flip mod 360 . (+) degree . fromDirection

forward :: Direction -> Int -> Move
forward East = GoEast
forward West = GoWest
forward North = GoNorth
forward South = GoSouth

toDirection :: Int -> Direction
toDirection 0 = North 
toDirection 90 = East 
toDirection 180 = South 
toDirection 270 = West 

fromDirection :: Direction -> Int
fromDirection North = 0
fromDirection East = 90
fromDirection South = 180
fromDirection West = 270

{-
https://adventofcode.com/2020/day/12

The navigation instructions (your puzzle input) consists of a sequence of single-character actions paired with integer input values. After staring at them for a few minutes, you work out what they probably mean:

Action N means to move north by the given value.
Action S means to move south by the given value.
Action E means to move east by the given value.
Action W means to move west by the given value.
Action L means to turn left the given number of degrees.
Action R means to turn right the given number of degrees.
Action F means to move forward by the given value in the direction the ship is currently facing.
The ship starts by facing east. Only the L and R actions change the direction the ship is facing. (That is, if the ship is facing east and the next instruction is N10, the ship would move north 10 units, but would still move east if the following action were F.)

For example:

F10
N3
F7
R90
F11
These instructions would be handled as follows:

F10 would move the ship 10 units east (because the ship starts by facing east) to east 10, north 0.
N3 would move the ship 3 units north to east 10, north 3.
F7 would move the ship another 7 units east (because the ship is still facing east) to east 17, north 3.
R90 would cause the ship to turn right by 90 degrees and face south; it remains at east 17, north 3.
F11 would move the ship 11 units south to east 17, south 8.
At the end of these instructions, the ship's Manhattan distance (sum of the absolute values of its east/west position and its north/south position) from its starting position is 17 + 8 = 25.

Figure out where the navigation instructions lead. What is the Manhattan distance between that location and the ship's starting position?
-}