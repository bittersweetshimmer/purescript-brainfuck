module Brainfuck.Cell (Cell, mkCell, cellToInt) where

import Data.Bounded (class Bounded, class Ord)
import Data.CommutativeRing (class CommutativeRing)
import Data.Eq (class Eq)
import Data.EuclideanRing (mod)
import Data.Function (($))
import Data.Ring (class Ring, (-))
import Data.Semigroup ((<>))
import Data.Semiring (class Semiring, (+), (*))
import Data.Show (class Show, show)

data Cell = Cell Int

mkCell :: Int -> Cell
mkCell n = Cell $ n `mod` 256

cellToInt :: Cell -> Int
cellToInt (Cell n) = n

derive instance eqCell :: Eq Cell

instance showCell :: Show Cell where
  show (Cell n) = "Cell " <> show n

derive instance ordCell :: Ord Cell

instance boundedCell :: Bounded Cell where
  top = Cell 255
  bottom = Cell 0

instance semiringCell :: Semiring Cell where
  add (Cell a) (Cell b) = mkCell $ a + b
  mul (Cell a) (Cell b) = mkCell $ a * b
  zero = Cell 0
  one = Cell 1

instance ringCell :: Ring Cell where
  sub (Cell a) (Cell b) = mkCell $ a - b

instance commutativeRingCell :: CommutativeRing Cell
