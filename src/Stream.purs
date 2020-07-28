module Brainfuck.Stream (
    Stream(..),
    streamOf,
    iterate,
    prepend,
    head,
    tail,
    (:>)
) where

import Control.Comonad (class Comonad, class Extend)
import Data.Function (($))
import Data.Lazy (Lazy, defer, force)
import Data.Show (class Show, show)
import Prelude (class Functor, map, (<>))

data Stream a = Stream a (Lazy (Stream a))

prepend :: forall a. a -> Stream a -> Stream a
prepend x s = Stream x $ defer $ \_ -> s

infixr 5 prepend as :>

streamExtract :: forall a. Stream a -> a
streamExtract (Stream h _) = h

streamDuplicate :: forall a. Stream a -> Stream (Stream a)
streamDuplicate s@(Stream h t) = Stream s $ defer $ \_ -> streamDuplicate $ force t

instance functorStream :: Functor Stream where
    map f (Stream h t) = Stream (f h) $ defer $ \_ -> map f $ force t

instance extendStream :: Extend Stream where
    extend f s = map f (streamDuplicate s)

instance comonadStream :: Comonad Stream where
    extract = streamExtract

instance showStream :: Show a => Show (Stream a) where
    show (Stream h _) = "Stream " <> show h

streamOf :: forall a. a -> Stream a
streamOf a = Stream a $ defer $ \_ -> streamOf a

iterate :: forall a. (a -> a) -> a -> Stream a
iterate f x = Stream x $ defer $ \_ -> iterate f x'
    where
      x' = f x

head :: forall a. Stream a -> a
head (Stream h _) = h

tail :: forall a. Stream a -> Stream a
tail (Stream _ t) = force t