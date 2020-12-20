{-# LANGUAGE OverloadedStrings #-}

module D19 (
    parseFile
  , Rule(..)
  , RuleId
  , RuleSet
  , Message
) where

import Data.List
import Data.Char
import Data.Attoparsec.Text
import Control.Applicative
import Data.Either
import qualified Data.Text.IO as T
import qualified Data.Map as M

type RuleId = Int
type RuleSet = M.Map RuleId [Rule]
type RuleDef = (RuleId, [Rule])
type Message = String 

data Rule
    = LITERAL Char
    | RULE [RuleId]
    deriving(Show, Eq)

parseFile :: String -> IO RuleSet
parseFile f = T.readFile f >>= (return . M.fromList . fromRight [] . parseOnly parseRules)

parseRules :: Parser [RuleDef]
parseRules = many $ ruleDefParser <* (endOfLine <|> endOfInput)

skipSeparator = skipWhile isSeparator

ruleDefParser :: Parser RuleDef
ruleDefParser = do
        n <- decimal
        char ':'
        skipSeparator
        rules <- ruleParser `sepBy1` (skipSeparator *> char '|' *> skipSeparator)
        return (n, rules)

ruleParser :: Parser Rule
ruleParser =
        (char '"' *> letter <* char '"' >>= (return . LITERAL))
    <|> (many1 (skipSeparator *> decimal) >>= (return . RULE))
