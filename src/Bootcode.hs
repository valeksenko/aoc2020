{-# LANGUAGE OverloadedStrings #-}

module Bootcode (
    parseCode
  , parseFile
  , mkBootcode
  , exec
  , Accumulator
  , Position
  , InstructionSet(..)
  , State(..)
) where

import qualified Data.Sequence as S
import Data.Foldable (toList)
import Data.List
import Data.Attoparsec.Text
import Control.Applicative
import Data.Either
import qualified Data.Text.IO as T

type Accumulator = Int
type Address = Int
type Position = Int

data InstructionSet
    = ACC Address
    | JMP Address
    | NOP Address
    deriving(Show, Eq)

data State =
    State {
        sPosition :: Position
      , sAccumulator :: Accumulator
      , sCode :: S.Seq InstructionSet
    } deriving(Show, Eq)


parseFile :: String -> IO [InstructionSet]
parseFile f = T.readFile f >>= (return . fromRight [] . parseOnly parseCode)

parseCode :: Parser [InstructionSet]
parseCode = many $ instructionSetParser <* endOfLine

instructionSetParser :: Parser InstructionSet
instructionSetParser =
        ("acc" *> char ' ' *> signed decimal >>= (return . ACC))
    <|> ("jmp" *> char ' ' *> signed decimal >>= (return . JMP))
    <|> ("nop" *> char ' ' *> signed decimal >>= (return . NOP))

mkBootcode :: [InstructionSet] -> State
mkBootcode = State 0 0 . S.fromList

exec :: State -> State
exec state = execOp state $ (sCode state) `S.index` (sPosition state)

execOp :: State -> InstructionSet -> State
execOp state (ACC a) = state { sPosition = (sPosition state) + 1, sAccumulator = (sAccumulator state) + a }
execOp state (JMP a) = state { sPosition = (sPosition state) + a }
execOp state (NOP _) = state { sPosition = (sPosition state) + 1 }
