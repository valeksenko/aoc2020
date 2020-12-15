{-# LANGUAGE OverloadedStrings #-}

module D14 (
    parseFile
  , InstructionSet(..)
  , Address
  , Register
  , BitMask
) where

import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Attoparsec.Text
import Control.Applicative
import Data.Either

type Address = Int
type Register = Integer
type BitMask = T.Text

data InstructionSet
    = Mem (Address, Register) 
    | Mask BitMask
    deriving(Show, Eq)

parseFile :: String -> IO [InstructionSet]
parseFile f = TIO.readFile f >>= (return . fromRight [] . parseOnly parseCode)

parseCode :: Parser [InstructionSet]
parseCode = many $ instructionSetParser <* endOfLine

memParser :: Parser InstructionSet
memParser = do
    string "mem["
    addr <- decimal
    string "] = "
    val <- decimal
    return $ Mem (addr, val)

mask = inClass "X01"

instructionSetParser :: Parser InstructionSet
instructionSetParser = ("mask = " *> takeWhile1 mask >>= (return . Mask)) <|> memParser
