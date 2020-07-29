module Test.Main where

import Prelude

import Brainfuck (evalMock)
import Brainfuck.Cell (mkCell)
import Brainfuck.Command (Command(..))
import Brainfuck.Console.Mock (Mock)
import Brainfuck.Program (Program(..), parseProgram)
import Brainfuck.Stream (Stream, head, iterate, prepend, streamOf, tail, (:>))
import Brainfuck.Tape (Tape(..), backward, forward)
import Control.Comonad (extract)
import Data.Array (reverse)
import Data.Char (toCharCode)
import Data.List (List(..), (:), fromFoldable)
import Data.Maybe (Maybe(..), maybe)
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Test.Unit (Test, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

testProgram :: Mock -> Program -> Array Int -> Test
testProgram mock program output = Assert.equal (evalMock mock program).output $ output # reverse # fromFoldable <#> mkCell

main :: Effect Unit
main = runTest do
  suite "Cell" do
    test "show" $ Assert.equal "Cell 10" (show $ mkCell 10)
    test "mkCell n creates Cell (n % 256)" $ Assert.equal (mkCell 1337) (mkCell $ 1337 `mod` 256)
    suite "Bounded Cell" do
      test "bottom of Cell is 0" $ Assert.equal (mkCell 0) $ bottom
      test "top of Cell is 255" $ Assert.equal (mkCell 255) $ top
    suite "Semiring Cell" do
      test "zero of Cell is 0" $ Assert.equal (mkCell 0) $ zero
      test "one of Cell is 1" $ Assert.equal (mkCell 1) $ one
      test "addition wraps around 256" $ Assert.equal (mkCell 20 + mkCell 250) $ mkCell (mod (20 + 250) 256)
      test "multiplication wraps around 256" $ Assert.equal (mkCell 20 * mkCell 20) $ mkCell (mod (20 * 20) 256)
    suite "Ring Cell" do
      test "difference wraps around 0" $ Assert.equal (mkCell 1 - mkCell 2) $ mkCell 255
    suite "CommutativeRing Cell" do
      test "multiplication is commutative" $ Assert.equal (mkCell 69 * mkCell 42) $ (mkCell 42) * (mkCell 69)
  
  suite "Stream" do
    test "head" $ Assert.equal (head $ streamOf 0) 0
    test "prepend" $ Assert.equal (head $ prepend 69 $ streamOf 0) 69
    test "tail" $ Assert.equal (head $ tail $ prepend 69 $ streamOf 0) 0
    test "iterate - stream of natural numbers" do
      let stream = iterate (add 1) 0 :: Stream Int
      Assert.equal (head stream) 0
      Assert.equal (head $ tail stream) 1
      Assert.equal (head $ tail $ tail stream) 2
    test "map - stream of squared natural number" do
      let stream = map (\x -> x * x) (iterate (add 1) 0) :: Stream Int
      Assert.equal (head stream) 0
      Assert.equal (head $ tail stream) 1
      Assert.equal (head $ tail $ tail stream) 4

  suite "Tape" do
    let tape = Tape { left: 42 :> streamOf 0, focus: 69, right: 1 :> streamOf 0 }
    test "extract" $ Assert.equal (extract tape) 69
    test "forward" $ Assert.equal (extract $ forward tape) 1
    test "backward" $ Assert.equal (extract $ backward tape) 42
    test "map" $ do
      let mapped = map (add 1) tape
      Assert.equal (extract mapped) 70
      Assert.equal (extract $ forward mapped) 2
      Assert.equal (extract $ backward mapped) 43
  
  suite "Program" do
    test "parsing \"+-<>[].,\"" do
      let source = "+-<>[].,"
      Assert.equal (parseProgram source) $ Just $ Program (Increment : Decrement : DecrementPointer : IncrementPointer : JumpZero : JumpNonZero : Write : Read : Nil)
    test "parsing \"+-<>foo\"" do
      let source = "+-<>foo"
      Assert.equal (parseProgram source) Nothing
  
  suite "Brainfuck" do
    test ".+.,.>. outputs 0, 1, 42, 0" do
      let program = Program (Write : Increment : Write : Read : Write : IncrementPointer : Write : Nil) 
      let mock = { output: Nil, input: mkCell 42 :> mkCell 69 :> mkCell 1 :> streamOf zero }
      testProgram mock program [0, 1, 42, 0]
    test "\"Hello world\" program parses correctly and outputs \"hello world\"" do
      let helloworld = parseProgram "+[-[<<[+[--->]-[<<<]]]>>>-]>-.---.>..>.<<<<-.<+.>>>>>.>.<<.<-."
      let mock = { output: Nil, input: mkCell 42 :> mkCell 69 :> mkCell 1 :> streamOf zero }

      maybe (Assert.assert "hello world parsing failure" false) (\program -> testProgram mock program $ "hello world" # toCharArray <#> toCharCode) helloworld