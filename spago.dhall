{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-brainfuck"
, dependencies =
  [ "aff"
  , "assert"
  , "console"
  , "effect"
  , "free"
  , "lists"
  , "node-process"
  , "node-readline"
  , "psci-support"
  , "run"
  , "strings"
  , "test-unit"
  , "transformers"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
