module Brainfuck.Tape (Tape(..), backward, forward, tapeOf) where

import Brainfuck.Stream (Stream, head, iterate, prepend, streamOf, tail)
import Control.Comonad (class Comonad, class Extend)
import Data.Show (class Show, show)
import Prelude (class Functor, map, (<>))

newtype Tape a = Tape { left :: Stream a, focus :: a, right :: Stream a }

backward :: forall a. Tape a -> Tape a
backward (Tape { left, focus, right }) = Tape { left: tail left, focus: head left, right: prepend focus right}

forward :: forall a. Tape a -> Tape a
forward (Tape { left, focus, right }) = Tape { left: prepend focus left, focus: head right, right: tail right}

tapeExtract :: forall a. Tape a -> a
tapeExtract (Tape { focus }) = focus

tapeDuplicate :: forall a. Tape a -> Tape (Tape a)
tapeDuplicate tape = Tape { focus: tape, left: iterate forward tape, right: iterate backward tape }

instance functorTape :: Functor Tape where
    map f (Tape { left, focus, right}) = Tape { left: map f left, focus: f focus, right: map f right }

instance extendTape :: Extend Tape where
    extend f s = map f (tapeDuplicate s)

instance comonadTape :: Comonad Tape where
    extract = tapeExtract

instance showTape :: Show a => Show (Tape a) where
    show (Tape { focus }) = "Tape " <> show focus

tapeOf :: forall a. a -> Tape a
tapeOf a = Tape { focus: a, left: streamOf a, right: streamOf a }