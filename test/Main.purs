module Test.Main where

import Prelude

import Brainfuck (runMock)
import Brainfuck.Cell (mkCell)
import Brainfuck.Command (Command(..))
import Brainfuck.Program (Program(..), parseProgram)
import Brainfuck.Step (Step(..))
import Brainfuck.Stream (Stream, head, iterate, prepend, streamOf, tail, (:>))
import Brainfuck.Tape (Tape(..), backward, forward)
import Control.Comonad (extract)
import Data.Array (reverse)
import Data.Char (toCharCode)
import Data.List (List(..), (:), fromFoldable)
import Data.Maybe (Maybe(..), isJust)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

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
    test "map - stream of squared natural numbers" do
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
    suite "Program \"[\"" do
      let program = Program (JumpZero : Nil) 
      let mock = { output: Nil, input: streamOf 0 <#> mkCell }
      let Tuple _ (Tuple _ step) = runMock mock program

      test "ends with invalid jump from position 0" $ Assert.equal step (InvalidJump 0)
    suite "Program \".+.,.>,.\" with input stream 42, 69, 1, 0" do
      let program = Program (Write : Increment : Write : Read : Write : IncrementPointer : Read : Write : Nil) 
      let mock = { output: Nil, input: 42 :> 69 :> 1 :> streamOf 0 <#> mkCell }
      let Tuple { output } (Tuple state step) = runMock mock program

      test "outputs 0, 1, 42, 69" $ Assert.equal output $ [0, 1, 42, 69] # reverse # fromFoldable <#> mkCell
      test "ends with counter value of 8" $ Assert.equal state.counter 8
      test "ends with cell value of 69" $ Assert.equal (extract state.tape) (mkCell 69)
      test "ends successfully" $ Assert.equal step End
    suite "Program \",[.,]\"" do
      let program = Program (Read : JumpZero : Write : Read : JumpNonZero : Nil)
      let mock = { output: Nil, input: 69 :> 1 :> 42 :> streamOf 0 <#> mkCell }
      let Tuple { output } (Tuple state step) = runMock mock program

      test "pipes its input to output until 0 is entered" $ Assert.equal output $ [69, 1, 42] # reverse # fromFoldable <#> mkCell
      test "ends successfully" $ Assert.equal step End
      test "ends with cell value of 0" $ Assert.equal (extract state.tape) zero
    suite "A hello world program" do
      let helloworld = parseProgram "+[-[<<[+[--->]-[<<<]]]>>>-]>-.---.>..>.<<<<-.<+.>>>>>.>.<<.<-."
      let mock = { output: Nil, input: streamOf 0 <#> mkCell }

      test "parses successfully" $ Assert.equal (isJust helloworld) true

      case helloworld of
        Just program -> do
          let Tuple { output } (Tuple state step) = runMock mock program

          test "outputs \"hello world\"" $ Assert.equal output $ "hello world" # toCharArray <#> toCharCode # reverse # fromFoldable <#> mkCell
          test "ends successfully" $ Assert.equal step End
        Nothing -> pure unit