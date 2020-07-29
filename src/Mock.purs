module Brainfuck.Console.Mock where

import Prelude

import Brainfuck.Console (CONSOLE, ConsoleF, _console)
import Brainfuck.Console (ConsoleF(..)) as Console
import Brainfuck.Stream (Stream(..))
import Brainfuck.Cell (Cell, cellToInt)
import Data.Char (fromCharCode)
import Data.Lazy (force)
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (singleton)
import Data.Tuple (Tuple)
import Run (Run, interpret, on, send)
import Run.State (STATE, get, put, runState)

type Mock = { output :: String, input :: Stream Cell }

runMockConsole :: forall r a. Mock -> Run (state :: STATE Mock, console :: CONSOLE | r) a -> Run r (Tuple Mock a)
runMockConsole mocked x = x # interpret (on _console handler send) # runState mocked
    where
        handler :: forall s. ConsoleF ~> Run (state :: STATE Mock | s)
        handler (Console.Write cell next) = do
            mock <- get
            put $ mock { output = mock.output <> (singleton $ fromMaybe ' ' $ fromCharCode $ cellToInt cell) }
            pure next
        handler (Console.Read reply) = do
            mock <- get
            let (Stream h t) = mock.input
            put $ mock { input = force t }
            pure $ reply h