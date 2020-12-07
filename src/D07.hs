{-# LANGUAGE OverloadedStrings #-}

module D07 (
    rulesParser
  , Rule
  , Bag
  , Content
) where

import Data.List
import Data.Attoparsec.Text
import Control.Applicative

type Bag = (String, String)
type Content = (Bag, Int)
type Rule = (Bag, [Content])

rulesParser :: Parser [Rule]
rulesParser = many $ ruleParser <* endOfLine

ruleParser :: Parser Rule
ruleParser = do
    bag <- bagParser
    string " contain "
    contents <- contentsParser
    return (bag, contents)

bagParser :: Parser Bag
bagParser = do
    qulifier <- many1 letter
    char ' '
    color <- many1 letter
    string " bags" <|> string " bag"
    return (qulifier, color)

contentsParser :: Parser [Content]
contentsParser = (string "no other bags." >> return []) <|> (many $ contentParser <* (string ", " <|> string "."))

contentParser :: Parser Content
contentParser = do
    amount <- decimal
    char ' '
    bag <- bagParser
    return (bag, amount)
