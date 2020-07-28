module Brainfuck.State where

import Prelude

import Brainfuck.Cell (Cell)
import Brainfuck.Program (Program)
import Brainfuck.Tape (Tape, tapeOf)

data State = State { tape :: Tape Cell, program :: Program, instruction :: Int, buffer :: String }

instance showState :: Show State where
    show :: State -> String
    show (State { tape, program, instruction, buffer }) = "State { " <> show tape <> ", " <> show program <> ", " <> show instruction <> ", " <> show buffer <> " }"

mkState :: Program -> State
mkState program = State { tape: tapeOf zero, program, instruction: 0, buffer: "" }