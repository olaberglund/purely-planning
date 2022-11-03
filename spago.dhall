{ name = "halogen-project"
, dependencies =
  [ "arrays"
  , "console"
  , "datetime"
  , "effect"
  , "enums"
  , "halogen"
  , "halogen-formless"
  , "maybe"
  , "now"
  , "prelude"
  , "psci-support"
  , "aff"
  , "dom-indexed"
  , "either"
  , "integers"
  , "strings"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
