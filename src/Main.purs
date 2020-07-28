module Main where

import Prelude

import Brainfuck as Brainfuck
import Brainfuck.State (State(..))
import Brainfuck.Step (Step(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
    execution <- Brainfuck.run "+[-[<<[+[--->]-[<<<]]]>>>-]>-.---.>..>.<<<<-.<+.>>>>>.>.<<.<-."
    case execution of
        Just (Tuple (InvalidJump n) _) -> log $ "Error: Invalid jump at position " <> show n <> "."
        Just (Tuple _ (State { buffer })) -> log $ "Success: " <> buffer
        _ -> log "Error: Parsing failed."