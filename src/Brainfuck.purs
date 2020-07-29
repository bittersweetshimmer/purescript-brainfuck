module Brainfuck where

import Prelude

import Brainfuck.Cell (Cell)
import Brainfuck.Command (Command(..)) as Command
import Brainfuck.Command (Command)
import Brainfuck.Console (CONSOLE, read, runConsole, write)
import Brainfuck.Console.Mock (Mock, runMockConsole)
import Brainfuck.Program (Program(..))
import Brainfuck.State (State, initialState)
import Brainfuck.Step (Step(..))
import Brainfuck.Tape (Tape(..))
import Brainfuck.Tape (forward, backward) as Tape
import Data.List ((!!))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Run (Run, extract, runBaseAff)
import Run.Reader (READER, ask, runReader)
import Run.State (STATE, get, modify, put, runState)

jump :: forall r. Int -> Run (state :: STATE State | r) Unit
jump n = get >>= \state -> put (state { counter = n })

setCell :: forall r. Cell -> Run (state :: STATE State | r) Unit
setCell c = modify $ \state@{ tape: (Tape tape@{ focus }) } -> state { tape = (Tape (tape { focus = c })) }

getCell :: forall r. Run (state :: STATE State | r) Cell
getCell = get >>= \{ tape: (Tape { focus }) } -> pure focus

modifyCell :: forall r. (Cell -> Cell) -> Run (state :: STATE State | r) Unit
modifyCell f = do
    cell <- getCell
    setCell $ f cell

getCounter :: forall r. Run (state :: STATE State | r) Int
getCounter = get >>= \{ counter } -> pure counter

incrementCounter :: forall r. Run (state :: STATE State | r) Unit
incrementCounter = do
    counter <- getCounter
    jump $ counter + 1

forward :: forall r. Run (state :: STATE State | r) Unit
forward = modify $ \state@{ tape } -> state { tape = Tape.forward tape }

backward :: forall r. Run (state :: STATE State | r) Unit
backward = modify $ \state@{ tape } -> state { tape = Tape.backward tape }

getCommandAt :: forall r. Int -> Run (state :: STATE State, reader :: READER Program | r) (Maybe Command)
getCommandAt n = do
    Program commands <- ask
    pure $ commands !! n

getCommand :: forall r. Run (state :: STATE State, reader :: READER Program | r) (Maybe Command)
getCommand = do
    index <- getCounter
    Program commands <- ask
    pure $ commands !! index

findLoopEnd :: forall r. Run (state :: STATE State, reader :: READER Program | r) (Maybe Int)
findLoopEnd = do
    from <- getCounter
    result <- go (from + 1) 0
    pure $ result
    where 
        go :: Int -> Int -> Run (state :: STATE State, reader :: READER Program | r) (Maybe Int)
        go i depth = do
            maybeCommand <- getCommandAt i
            case maybeCommand of 
                Just c -> case c of
                    Command.JumpNonZero -> if depth == 0
                        then pure $ Just $ i + 1
                        else go (i + 1) (depth - 1)
                    Command.JumpZero -> go (i + 1) (depth + 1)
                    _ -> go (i + 1) depth
                Nothing -> pure $ Nothing

findLoopStart :: forall r. Run (state :: STATE State, reader :: READER Program | r) (Maybe Int)
findLoopStart = do
    from <- getCounter
    result <- go (from - 1) 0
    pure $ result
    where 
        go :: Int -> Int -> Run (state :: STATE State, reader :: READER Program | r) (Maybe Int)
        go i depth = do
            maybeCommand <- getCommandAt i
            case maybeCommand of 
                Just c -> case c of
                    Command.JumpZero -> if depth == 0
                        then pure $ Just $ i + 1
                        else go (i - 1) (depth - 1)
                    Command.JumpNonZero -> go (i - 1) (depth + 1)
                    _ -> go (i - 1) depth
                Nothing -> pure $ Nothing

continue :: forall f. Applicative f => f Step
continue = pure Continue

end :: forall f. Applicative f => f Step
end = pure End

invalid :: forall f. Applicative f => Int -> f Step
invalid = pure <<< InvalidJump

executeCommand :: forall r. Command -> Run (console :: CONSOLE, state :: STATE State, reader :: READER Program | r) Step
executeCommand Command.Write = do
    getCell >>= write
    incrementCounter
    continue
executeCommand Command.Read = do
    read >>= setCell
    incrementCounter
    continue
executeCommand Command.IncrementPointer = do
    forward
    incrementCounter
    continue
executeCommand Command.DecrementPointer = do
    backward
    incrementCounter
    continue
executeCommand Command.Increment = do
    modifyCell $ add one
    incrementCounter
    continue
executeCommand Command.Decrement = do
    modifyCell $ flip sub one
    incrementCounter
    continue
executeCommand Command.JumpZero = do
    value <- getCell
    from <- getCounter
    if value == zero
        then do
            found <- findLoopEnd
            case found of
                Just to -> do
                    jump to
                    continue
                Nothing -> do
                    invalid from
        else do
            jump (from + 1)
            continue
executeCommand Command.JumpNonZero = do
    value <- getCell
    from <- getCounter
    if not $ value == zero
        then do
            found <- findLoopStart
            case found of
                Just to -> do
                    jump to
                    continue
                Nothing -> do
                    invalid from
        else do
            jump (from + 1)
            continue

step :: forall r. Run (console :: CONSOLE, state :: STATE State, reader :: READER Program | r) Step
step = getCommand >>= maybe end executeCommand

brainfuck :: forall r. Run (console :: CONSOLE, state :: STATE State, reader :: READER Program | r) Step
brainfuck = do
    result <- step
    case result of
        Continue -> brainfuck
        _ -> pure $ result

runBrainfuck :: Program -> Aff (Tuple State Step)
runBrainfuck program = brainfuck # runState initialState # runReader program # runConsole # runBaseAff

runMock :: Mock -> Program -> Tuple Mock (Tuple State Step)
runMock mock program = brainfuck # runState initialState # runReader program # runMockConsole mock # extract

evalMock :: Mock -> Program -> Mock
evalMock mock program = let Tuple m _ = runMock mock program in m