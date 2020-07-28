module Brainfuck.Step where

import Prelude

data Step = Continue | InvalidJump Int | End

instance showStep :: Show Step where
    show :: Step -> String
    show Continue = "Continue "
    show (InvalidJump n) = "InvalidJump " <> show n
    show End = "End"