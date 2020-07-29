module Brainfuck.Console where

import Prelude

import Brainfuck.Cell (Cell, cellToInt, mkCell)
import Data.Array (index)
import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.String.CodeUnits (singleton, toCharArray)
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)
import Effect.Aff (Aff, Canceler(..), makeAff)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..)) as Encoding
import Node.Process (stdout)
import Node.ReadLine (close, createConsoleInterface, noCompletion, question)
import Node.Stream (writeString)
import Run (AFF, Run, interpret, lift, liftAff, on, send)

data ConsoleF a = Write Cell a | Read (Cell -> a)

derive instance functorConsoleF :: Functor ConsoleF

type CONSOLE = FProxy ConsoleF
_console = SProxy :: SProxy "console"

write :: forall r. Cell -> Run (console :: CONSOLE | r) Unit
write c = lift _console (Write c unit)

read :: forall r. Run (console :: CONSOLE | r) Cell
read = lift _console (Read identity)

runConsole :: forall r. Run (aff :: AFF, console :: CONSOLE | r) ~> Run (aff :: AFF | r)
runConsole = interpret (on _console handler send)
    where
        handler :: ConsoleF ~> Run (aff :: AFF | r)
        handler (Write cell next) = do
            liftAff $ stdoutWrite cell
            pure next
        handler (Read reply) = do
            str <- liftAff $ stdinRead
            pure <<< reply $ maybe zero (mkCell <<< toCharCode) (str # toCharArray # flip index 0)

        stdoutWrite :: Cell -> Aff Unit
        stdoutWrite cell = liftEffect $ void $ writeString stdout Encoding.UTF8 (maybe " " singleton $ fromCharCode $ cellToInt cell) (pure unit)

        stdinRead :: Aff String
        stdinRead = makeAff $ \resolve -> do
            interface <- createConsoleInterface noCompletion
            question "" (resolve <<< Right >=> const (close interface)) interface
            pure $ Canceler $ const $ liftEffect $ close interface