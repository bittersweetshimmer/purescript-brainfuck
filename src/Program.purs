module Brainfuck.Program where

import Brainfuck.Command (Command, parseCommand)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List (List, fromFoldable)
import Data.Maybe (Maybe)
import Data.Show (class Show)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)
import Prelude (class Eq, show, (<>))

newtype Program = Program (List Command)

instance showProgram :: Show Program where
  show :: Program -> String
  show (Program commands) = "Program " <> show commands

derive instance eqProgram :: Eq Program

parseProgram :: String -> Maybe Program
parseProgram s = Program <$> sequence (fromFoldable $ parseCommand <$> toCharArray s)