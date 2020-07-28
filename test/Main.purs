module Test.Main where

import Prelude

import Brainfuck.Cell (Cell, mkCell)
import Brainfuck.Stream (Stream, head, iterate, prepend, tail)
import Brainfuck.Tape (Tape, tapeOf)
import Control.Comonad (extract)
import Effect (Effect)
import Test.Assert (assert, assertEqual)

main :: Effect Unit
main = do
  assertEqual { expected: "Cell 10", actual: show (mkCell 10) }
  assertEqual { expected: mkCell 1337, actual: mkCell (1337 `mod` 256) }
  assertEqual { expected: mkCell 0, actual: bottom }
  assertEqual { expected: mkCell 255, actual: top }
  assertEqual { expected: mkCell 1, actual: one }
  assertEqual { expected: zero :: Cell, actual: bottom }
  assertEqual { expected: bottom - one :: Cell, actual: top }
  assertEqual { expected: top + one :: Cell, actual: bottom }
  assertEqual { expected: mkCell 100 * mkCell 100, actual: mkCell (100 * 100) }

  let stream = iterate (add one) zero :: Stream Cell
  assert $ head stream == zero 
  assert $ head (tail stream) == one
  assert $ head (prepend (mkCell 69) stream) == mkCell 69 

  let tape = tapeOf zero :: Tape Cell
  assert $ extract tape == zero