module Brainfuck.Natural (mkNatural, Natural(..), naturalToInt) where

import Data.Eq (class Eq)
import Data.Function (($))
import Data.Ord (class Ord, (>))
import Data.Show (show)
import Data.Ring ((-))
import Data.Semiring ((+))
import Data.Semigroup ((<>))
import Partial (crash)
import Partial.Unsafe (unsafePartial)

data Natural = Zero | Succ Natural

mkNatural :: Int -> Natural
mkNatural n = unsafePartial $ mkNatural' n
    where 
        mkNatural' :: Partial => Int -> Natural
        mkNatural' 0 = Zero
        mkNatural' x = if x > 0
            then Succ $ mkNatural' (x - 1)
            else crash $ show x <> " is not a natural number."

naturalToInt :: Natural -> Int
naturalToInt Zero = 0
naturalToInt (Succ n) = naturalToInt n + 1

derive instance eqNatural :: Eq Natural
derive instance ordNatural :: Ord Natural