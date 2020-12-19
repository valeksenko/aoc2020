module D18 (
    parseStatement
  , Statement(..)
) where

import Data.List

data Statement
    = NUM Int
    | ADD
    | MUL
    | START
    | END
    deriving(Show, Eq)

parseStatement :: String -> [Statement]
parseStatement = foldr mapChar []
    where
        mapChar c l = case c of
            ' ' -> l
            '+' -> ADD:l
            '*' -> MUL:l
            '(' -> START:l
            ')' -> END:l
            otherwise -> (NUM (read [c] :: Int)):l
