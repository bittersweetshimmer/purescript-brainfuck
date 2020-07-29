module Main where

import Prelude

import Brainfuck (runBrainfuck) as Brainfuck
import Brainfuck.Program (parseProgram) as Brainfuck
import Data.Array (index)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Node.Process (argv) as Process

main :: Effect Unit
main = Process.argv <#> flip index 2 >>= case _ of
    Just src -> case Brainfuck.parseProgram src of
        Just program -> launchAff_ $ Brainfuck.runBrainfuck program
        Nothing -> log "Error: Invalid character."
    _ -> log "Error: No arguments."
                    
