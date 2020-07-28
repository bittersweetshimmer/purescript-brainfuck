module Brainfuck.State where

import Prelude

import Brainfuck.Cell (Cell)
import Brainfuck.Tape (Tape, tapeOf)

type State = { tape :: Tape Cell, counter :: Int }

initialState :: State
initialState = { tape: tapeOf zero, counter: 0 }