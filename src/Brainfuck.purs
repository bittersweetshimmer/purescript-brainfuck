module Brainfuck where

import Brainfuck.Cell (Cell, cellToInt)
import Brainfuck.Command (Command(..))
import Brainfuck.Natural (Natural(..))
import Brainfuck.Program (Program(..), parseProgram)
import Brainfuck.State (State(..), mkState)
import Brainfuck.Step (Step(..))
import Brainfuck.Tape (Tape(..))
import Brainfuck.Tape (forward, backward) as T
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Comonad (extract)
import Control.Monad.State (StateT, evalStateT, execStateT, lift, runStateT, state)
import Data.Char (fromCharCode)
import Data.Eq ((==))
import Data.Function (const, ($), (<<<), (>>>), flip)
import Data.HeytingAlgebra (not)
import Data.List ((!!))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Ring (sub, (-))
import Data.Semigroup ((<>))
import Data.Semiring (add, one, zero, (+))
import Data.String (singleton, codePointFromChar)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Class.Console (log)

type Brainfuck a = StateT State Effect a

getCell :: Brainfuck Cell
getCell = state $ \s@(State { tape }) -> Tuple (extract tape) s

getBuffer :: Brainfuck String
getBuffer = state $ \s@(State { buffer }) -> Tuple buffer s

clearBuffer :: Brainfuck Unit
clearBuffer = state $ \(State { tape, program, instruction, buffer }) -> Tuple unit (State { tape, program, instruction, buffer: "" })

getProgram :: Brainfuck Program
getProgram = state $ \s@(State { program }) -> Tuple program s

getCommand :: Brainfuck (Maybe Command)
getCommand = state $ \s@(State { program: Program commands, instruction }) -> Tuple (commands !! instruction) s

modifyCell :: (Cell -> Cell) -> Brainfuck Unit
modifyCell f = state $ \(State { tape: (Tape { focus, left, right }), program, instruction, buffer }) -> Tuple unit (State { tape: Tape { focus: f focus, left, right }, program, instruction, buffer })

forward :: Brainfuck Unit
forward = state $ \(State { tape, program, instruction, buffer }) -> Tuple unit (State { tape: T.forward tape, program, instruction, buffer })

backward :: Brainfuck Unit
backward = state $ \(State { tape, program, instruction, buffer }) -> Tuple unit (State { tape: T.backward tape, program, instruction, buffer })

increment :: Brainfuck Unit
increment = state $ \(State { tape, program, instruction, buffer }) -> Tuple unit (State { tape, program, instruction: instruction + 1, buffer })

getInstruction :: Brainfuck Int
getInstruction = state $ \s@(State { instruction }) -> Tuple instruction s

getCommandAt :: Int -> Brainfuck (Maybe Command)
getCommandAt n = state $ \s@(State { program: Program commands }) -> Tuple (commands !! n) s

write :: Char -> Brainfuck Unit
write c = state $ \(State { tape, program, instruction, buffer }) -> Tuple unit (State { tape, program, instruction, buffer: buffer <> (singleton $ codePointFromChar $ c) })

flush :: Brainfuck Unit
flush = do
    buffer <- getBuffer
    log >>> lift $ buffer
    clearBuffer

continue :: Brainfuck Step
continue = pure Continue

invalid :: Int -> Brainfuck Step
invalid = pure <<< InvalidJump

end :: Brainfuck Step
end = pure End

next :: Brainfuck Step
next = increment >>= const continue

jump :: Int -> Brainfuck Step
jump n = state $ \(State { tape, program, instruction, buffer }) -> Tuple Continue (State { tape, program, buffer, instruction: n })

findLoopEnd :: Brainfuck (Maybe Int)
findLoopEnd = do
    instruction <- getInstruction
    go (instruction + 1) 0
    where
        go :: Int -> Int -> Brainfuck (Maybe Int)
        go i depth = getCommandAt i >>= \maybeCommand -> case maybeCommand of
            Just command -> case command of
                JumpNonZero -> if depth == 0
                    then pure $ Just $ i + 1
                    else go (i + 1) (depth - 1)
                JumpZero -> go (i + 1) (depth + 1)
                _ -> go (i + 1) depth
            _ -> pure $ Nothing

findLoopStart:: Brainfuck (Maybe Int)
findLoopStart = do
    instruction <- getInstruction
    go (instruction - 1) 0
    where
        go :: Int -> Int -> Brainfuck (Maybe Int)
        go i depth = getCommandAt i >>= maybe (pure Nothing) (\command -> case command of
            JumpZero -> if depth == 0
                then pure $ Just $ i + 1
                else go (i - 1) (depth - 1)
            JumpNonZero -> go (i - 1) (depth + 1)
            _ -> go (i - 1) depth)

runCommand :: Command -> Brainfuck Step
runCommand Increment = modifyCell (add one) >>= const next
runCommand Decrement = modifyCell (flip sub one) >>= const next
runCommand IncrementPointer = forward >>= const next
runCommand DecrementPointer = backward >>= const next
runCommand Write = getCell >>= cellToInt >>> fromCharCode >>> fromMaybe '?' >>> write >>= const next 
runCommand Read = next
runCommand JumpZero = do
    cell <- getCell
    from <- getInstruction
    if cell == zero
        then findLoopEnd >>= maybe (invalid from) jump
        else next
runCommand JumpNonZero = do
    cell <- getCell
    from <- getInstruction
    if not $ cell == zero
        then findLoopStart >>= maybe (invalid from) jump
        else next

step :: Brainfuck Step
step = getCommand >>= maybe end runCommand

steps :: Natural -> Brainfuck Step
steps Zero = continue
steps (Succ n) = do
    result <- step
    case result of
        Continue -> steps n
        _ -> pure $ result

brainfuck :: Brainfuck Step
brainfuck = step >>= \result -> case result of
    Continue -> brainfuck
    _ -> pure $ result

eval :: String -> Effect (Maybe String)
eval src = case parseProgram src of
    Just program -> do 
        buffer <- evalStateT (brainfuck >>= \_ -> getBuffer) $ mkState program
        pure $ Just $ buffer
    _ -> pure Nothing

exec :: String -> Effect (Maybe State)
exec src = case parseProgram src of
    Just program -> do 
        state <- execStateT brainfuck $ mkState program
        pure $ Just $ state
    _ -> pure Nothing

run :: String -> Effect (Maybe (Tuple Step State))
run src = case parseProgram src of
    Just program -> do 
        result <- runStateT brainfuck $ mkState program
        pure $ Just $ result
    _ -> pure Nothing