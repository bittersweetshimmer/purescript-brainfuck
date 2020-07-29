module Brainfuck.Command where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Show (class Show)

data Command =
    IncrementPointer |
    DecrementPointer |
    Increment |
    Decrement |
    Write |
    Read |
    JumpZero |
    JumpNonZero

instance showCommand :: Show Command where
    show :: Command -> String
    show IncrementPointer = "IncrementPointer"
    show DecrementPointer = "DecrementPointer"
    show Increment = "Increment"
    show Decrement = "Decrement"
    show Write = "Write"
    show Read = "Read"
    show JumpZero = "JumpZero"
    show JumpNonZero = "JumpNonZero"

derive instance eqProgram :: Eq Command

parseCommand :: Char -> Maybe Command
parseCommand '>' = Just IncrementPointer
parseCommand '<' = Just DecrementPointer
parseCommand '+' = Just Increment
parseCommand '-' = Just Decrement
parseCommand '.' = Just Write
parseCommand ',' = Just Read
parseCommand '[' = Just JumpZero
parseCommand ']' = Just JumpNonZero
parseCommand _ = Nothing