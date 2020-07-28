module Main where

import Prelude

import Brainfuck (evalMock) as Brainfuck
import Brainfuck.Console.Mock (Mock) as Brainfuck.Console
import Brainfuck.Program (Program(..), parseProgram) as Brainfuck
import Brainfuck.Stream (streamOf)

import Data.List (List(..))
import Data.Maybe (fromMaybe)

import Effect (Effect)
import Effect.Class.Console (log)

helloworld :: Brainfuck.Program
helloworld = fromMaybe (Brainfuck.Program Nil) (Brainfuck.parseProgram "+[-[<<[+[--->]-[<<<]]]>>>-]>-.---.>..>.<<<<-.<+.>>>>>.>.<<.<-.")

mock :: Brainfuck.Console.Mock
mock = { input: streamOf '?', output: "" }

result :: Brainfuck.Console.Mock
result = Brainfuck.evalMock mock helloworld

main :: Effect Unit
main = log result.output
