{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-brainfuck"
, dependencies =
  [ "assert"
  , "console"
  , "effect"
  , "lists"
  , "psci-support"
  , "strings"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
