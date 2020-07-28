module Brainfuck.Console where

import Prelude

import Data.String.CodeUnits (singleton)
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)
import Effect.Class.Console (log)
import Run (EFFECT, Run, interpret, lift, on, send)

data ConsoleF a = Write Char a | Read (Char -> a)

derive instance functorConsoleF :: Functor ConsoleF

type CONSOLE = FProxy ConsoleF
_console = SProxy :: SProxy "console"

write :: forall r. Char -> Run (console :: CONSOLE | r) Unit
write c = lift _console (Write c unit)

read :: forall r. Run (console :: CONSOLE | r) Char
read = lift _console (Read identity)

runConsole :: forall r. Run (effect :: EFFECT, console :: CONSOLE | r) ~> Run (effect :: EFFECT | r)
runConsole = interpret (on _console handler send)
    where
        handler :: ConsoleF ~> Run (effect :: EFFECT | r)
        handler (Write c next) = do
            log <<< singleton $ c
            pure next
        handler (Read reply) = do
            pure (reply 'a')
