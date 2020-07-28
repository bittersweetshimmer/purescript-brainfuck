{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-brainfuck"
, dependencies =
  [ "assert"
  , "console"
  , "effect"
  , "free"
  , "lists"
  , "psci-support"
  , "run"
  , "strings"
  , "transformers"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
