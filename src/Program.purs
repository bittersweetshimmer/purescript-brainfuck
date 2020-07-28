module Brainfuck.Program where

import Prelude ((<>), show)
import Brainfuck.Command (Command, parseCommand)
import Data.List (List, fromFoldable)
import Data.Show (class Show)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)

newtype Program = Program (List Command)

instance showProgram :: Show Program where
  show :: Program -> String
  show (Program commands) = "Program " <> show commands

parseProgram :: String -> Maybe Program
parseProgram s = Program <$> sequence (fromFoldable $ parseCommand <$> toCharArray s)