module Main where

import Debug.Trace
import Data.Either
import Data.Attoparsec.Text
import qualified Data.Text.IO as T
import D07
import D07P2

main :: IO ()
main = do
    -- f <- readFile "data/d06.txt"
    T.readFile "data/d07.txt" >>= (print . bagamount ("shiny","gold") . fromRight [] . parseOnly rulesParser)
    